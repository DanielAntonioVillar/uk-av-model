# data_loader.R --------------------------------------------------------
# Loading 2024 UK General Election results.
#
# Primary source: House of Commons Library "General Election 2024
# results" dataset (CC-BY). The HCL publishes a constituency-level
# CSV with vote counts by party.
#
# Because this app may run offline, we fall back to a bundled synthetic
# fixture that has the same schema and roughly plausible vote shares,
# so the UI is fully demonstrable without network access.
# ----------------------------------------------------------------------

# The 13 region keys we use (12 ITL1 + Northern Ireland).
REGIONS <- c(
  "North East", "North West", "Yorkshire and The Humber",
  "East Midlands", "West Midlands", "East of England",
  "London", "South East", "South West",
  "Scotland", "Wales", "Northern Ireland"
)

# Party codes used throughout the app. Order = display order.
GB_PARTIES <- c("Lab", "Con", "LD", "Reform", "Green", "SNP", "PC", "Other")
NI_PARTIES <- c("DUP", "SF", "SDLP", "UUP", "Alliance", "TUV", "Other")
ALL_PARTIES <- unique(c(GB_PARTIES, NI_PARTIES))

#' Load 2024 GE results from the House of Commons Library CSV
#'
#' @param path Local path to the HCL constituency results CSV. If NULL,
#'   the function will attempt to read from `data/hcl_2024.csv` relative
#'   to the app root.
#'
#' @return Data frame with columns: constituency_id, constituency_name,
#'   region, party, votes
load_hcl_2024 <- function(path = NULL) {
  if (is.null(path)) path <- "data/hcl_2024.csv"
  if (!file.exists(path)) {
    stop("HCL CSV not found at ", path,
         ". Download from the House of Commons Library and place there, ",
         "or use load_fixture() for the synthetic demo dataset.")
  }
  # The real HCL file is wide-format (one row per constituency, one
  # column per party). We pivot to long. Column names below match the
  # 2024 release; adjust if the HCL re-publishes with a new schema.
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("ONS ID", "Constituency name", "Region name")
  if (!all(required %in% names(raw))) {
    stop("HCL CSV missing expected columns: ",
         paste(setdiff(required, names(raw)), collapse = ", "))
  }
  party_cols <- intersect(
    c("Lab", "Con", "LD", "RUK", "Green", "SNP", "PC",
      "DUP", "SF", "SDLP", "UUP", "APNI", "TUV",
      "All other candidates", "Other"),
    names(raw)
  )
  long <- do.call(rbind, lapply(party_cols, function(col) {
    data.frame(
      constituency_id   = raw[["ONS ID"]],
      constituency_name = raw[["Constituency name"]],
      region            = raw[["Region name"]],
      party             = recode_party(col),
      votes             = suppressWarnings(as.numeric(raw[[col]])),
      row.names         = NULL,
      stringsAsFactors  = FALSE
    )
  }))
  long <- long[!is.na(long$votes) & long$votes > 0, ]
  long$region <- recode_region(long$region)
  # If multiple source columns mapped to the same party (e.g. both
  # "All other candidates" and a stray "Other" column), sum them.
  long <- aggregate(votes ~ constituency_id + constituency_name +
                    region + party, data = long, FUN = sum)
  long
}

recode_party <- function(x) {
  mapping <- c(RUK = "Reform",
               APNI = "Alliance",
               `All other candidates` = "Other")
  out <- mapping[x]
  ifelse(is.na(out), x, out)
}

recode_region <- function(x) {
  # HCL uses "Yorkshire and The Humber" in 2024; harmonise just in case.
  x <- gsub("Yorkshire and the Humber", "Yorkshire and The Humber", x,
            fixed = TRUE)
  x
}

#' Build a synthetic but plausible fixture for offline demo
#'
#' Generates ~50 constituencies per region with realistic-ish vote
#' shares. Not a substitute for real data, but lets the app run end
#' to end.
load_fixture <- function(seed = 2024, n_per_region = 50) {
  set.seed(seed)
  rows <- list()
  cid <- 1L

  # Rough regional 1st-preference averages (purely illustrative).
  base_shares <- list(
    "North East"               = c(Lab=.45, Con=.20, LD=.07, Reform=.18, Green=.05, Other=.05),
    "North West"               = c(Lab=.42, Con=.22, LD=.10, Reform=.15, Green=.06, Other=.05),
    "Yorkshire and The Humber" = c(Lab=.40, Con=.25, LD=.08, Reform=.17, Green=.05, Other=.05),
    "East Midlands"            = c(Lab=.35, Con=.30, LD=.08, Reform=.18, Green=.04, Other=.05),
    "West Midlands"            = c(Lab=.38, Con=.27, LD=.09, Reform=.16, Green=.05, Other=.05),
    "East of England"          = c(Lab=.30, Con=.32, LD=.13, Reform=.16, Green=.04, Other=.05),
    "London"                   = c(Lab=.47, Con=.20, LD=.13, Reform=.08, Green=.08, Other=.04),
    "South East"               = c(Lab=.28, Con=.32, LD=.18, Reform=.13, Green=.04, Other=.05),
    "South West"               = c(Lab=.26, Con=.30, LD=.20, Reform=.14, Green=.05, Other=.05),
    "Scotland"                 = c(Lab=.36, Con=.13, LD=.10, Reform=.07, Green=.04, SNP=.30, Other=.00),
    "Wales"                    = c(Lab=.37, Con=.18, LD=.07, Reform=.17, Green=.04, PC=.15, Other=.02),
    "Northern Ireland"         = c(DUP=.22, SF=.27, SDLP=.11, UUP=.13, Alliance=.15, TUV=.08, Other=.04)
  )

  for (region in REGIONS) {
    parties <- names(base_shares[[region]])
    means   <- base_shares[[region]]
    for (i in seq_len(n_per_region)) {
      # Dirichlet-style jitter via rgamma.
      alpha <- means * 50
      raw   <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
      shares <- raw / sum(raw)
      total  <- sample(35000:55000, 1)
      v <- round(shares * total)
      rows[[length(rows) + 1L]] <- data.frame(
        constituency_id   = sprintf("FX%05d", cid),
        constituency_name = sprintf("%s %02d", region, i),
        region            = region,
        party             = parties,
        votes             = v,
        stringsAsFactors  = FALSE
      )
      cid <- cid + 1L
    }
  }
  out <- do.call(rbind, rows)
  out[out$votes > 0, ]
}

#' Convenience: load real data if present, else fixture
#'
#' Priority: baked.rds (fastest, ships with deployed app) >
#'           hcl_2024.csv (raw CSV, for local development) >
#'           synthetic fixture (for offline demo).
load_results <- function() {
  if (file.exists("data/baked.rds")) {
    readRDS("data/baked.rds")$results
  } else if (file.exists("data/hcl_2024.csv")) {
    load_hcl_2024()
  } else {
    load_fixture()
  }
}

#' Load boundaries, preferring the baked version
load_boundaries <- function() {
  if (file.exists("data/baked.rds")) {
    return(readRDS("data/baked.rds")$boundaries)
  }
  # Fall back to a raw file in common formats.
  for (p in c("data/constituencies.gpkg", "data/constituencies.geojson",
              "data/constituencies.json")) {
    if (file.exists(p) && requireNamespace("sf", quietly = TRUE)) {
      return(sf::st_read(p, quiet = TRUE))
    }
  }
  NULL
}

#' Compute first-past-the-post winners for comparison
fptp_winners <- function(results_df) {
  do.call(rbind, lapply(split(results_df, results_df$constituency_id), function(d) {
    d[which.max(d$votes), c("constituency_id", "party")]
  })) |>
    setNames(c("constituency_id", "fptp_winner"))
}
