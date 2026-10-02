# flow_helpers.R -------------------------------------------------------
# Construct, validate, and apply preference-flow matrices.
# ----------------------------------------------------------------------

#' Default flow matrix for GB regions
#'
#' Rough plausible defaults loosely informed by BES/YouGov second-pref
#' polling. Users override these in the UI. Rows sum to 1.
default_gb_flows <- function() {
  parties <- c("Lab", "Con", "LD", "Reform", "Green", "SNP", "PC", "Other")
  m <- matrix(0, length(parties), length(parties),
              dimnames = list(parties, parties))
  set_row <- function(origin, prefs) {
    for (p in names(prefs)) m[origin, p] <<- prefs[[p]]
  }
  set_row("Lab",    list(LD = .35, Green = .30, Con = .05, Reform = .03, SNP = .12, PC = .10, Other = .05))
  set_row("Con",    list(Reform = .45, LD = .25, Lab = .10, Green = .05, SNP = .03, PC = .03, Other = .09))
  set_row("LD",     list(Lab = .45, Green = .25, Con = .15, Reform = .03, SNP = .05, PC = .05, Other = .02))
  set_row("Reform", list(Con = .55, Lab = .15, LD = .05, Green = .03, SNP = .05, PC = .05, Other = .12))
  set_row("Green",  list(Lab = .50, LD = .30, Con = .03, Reform = .02, SNP = .08, PC = .05, Other = .02))
  set_row("SNP",    list(Lab = .35, LD = .20, Green = .30, Con = .05, Reform = .05, PC = .03, Other = .02))
  set_row("PC",     list(Lab = .35, LD = .20, Green = .30, Con = .05, Reform = .05, SNP = .03, Other = .02))
  set_row("Other",  list(Lab = .25, Con = .20, LD = .20, Reform = .10, Green = .15, SNP = .05, PC = .05))
  m
}

#' Default flow matrix for Northern Ireland
default_ni_flows <- function() {
  parties <- c("DUP", "SF", "SDLP", "UUP", "Alliance", "TUV", "Other")
  m <- matrix(0, length(parties), length(parties),
              dimnames = list(parties, parties))
  set_row <- function(origin, prefs) {
    for (p in names(prefs)) m[origin, p] <<- prefs[[p]]
  }
  # Unionist bloc transfers within unionism; nationalist within nationalism;
  # Alliance receives transfers across both.
  set_row("DUP",      list(UUP = .50, TUV = .25, Alliance = .15, SDLP = .03, SF = .02, Other = .05))
  set_row("UUP",      list(DUP = .35, Alliance = .35, TUV = .10, SDLP = .10, SF = .03, Other = .07))
  set_row("TUV",      list(DUP = .60, UUP = .25, Alliance = .05, SDLP = .02, SF = .03, Other = .05))
  set_row("SF",       list(SDLP = .60, Alliance = .25, UUP = .03, DUP = .02, TUV = .02, Other = .08))
  set_row("SDLP",     list(SF = .40, Alliance = .40, UUP = .08, DUP = .03, TUV = .02, Other = .07))
  set_row("Alliance", list(SDLP = .30, UUP = .25, SF = .20, DUP = .10, TUV = .03, Other = .12))
  set_row("Other",    list(Alliance = .30, SDLP = .20, UUP = .15, SF = .15, DUP = .10, TUV = .05))
  m
}

#' Build the named list of flow matrices, one per region
#'
#' GB regions all share the default GB matrix unless overridden.
#' NI has its own matrix. Users override per-region in the UI.
default_flows_by_region <- function() {
  out <- list()
  gb <- default_gb_flows()
  ni <- default_ni_flows()
  for (r in REGIONS) {
    out[[r]] <- if (r == "Northern Ireland") ni else gb
  }
  out
}

#' Normalise each row of a flow matrix to sum to 1
normalise_flows <- function(m) {
  rs <- rowSums(m)
  rs[rs == 0] <- 1  # leave all-zero rows alone (engine will fall back)
  sweep(m, 1, rs, "/")
}

#' Construct a flow matrix from a long data frame of (origin, target, weight)
#'
#' Used to translate Shiny slider inputs into the matrix form.
flows_from_long <- function(long_df, parties) {
  m <- matrix(0, length(parties), length(parties),
              dimnames = list(parties, parties))
  for (i in seq_len(nrow(long_df))) {
    o <- long_df$origin[i]; t <- long_df$target[i]
    if (o %in% parties && t %in% parties && o != t) {
      m[o, t] <- long_df$weight[i]
    }
  }
  normalise_flows(m)
}
