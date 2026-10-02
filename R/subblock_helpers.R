# subblock_helpers.R ---------------------------------------------------
# Construct, validate, and convert sub-block specifications.
#
# A sub-block specification for one origin party is a data.frame with:
#   share:   numeric (will be normalised within an origin to sum to 1)
#   ranking: list-column of character vectors (2nd pref onward)
#
# A region's full sub-block spec is a named list of these data frames,
# one entry per origin party that the user has defined sub-blocks for.
# Parties without an entry fall back to default_flows.
# ----------------------------------------------------------------------

#' Build a default single-sub-block spec for one origin party
#'
#' Uses the flow matrix to derive the ranking, then returns a one-row
#' sub-block data frame with share = 1.
default_subblock_for <- function(origin, flows) {
  parties <- rownames(flows)
  rest <- ranking_from_flow(origin, flows[origin, ])
  if (length(rest) == 0) rest <- sort(setdiff(parties, origin))
  df <- data.frame(share = 1.0, stringsAsFactors = FALSE)
  df$ranking <- list(rest)
  df
}

#' Build a default region spec: one sub-block per origin party,
#' matching what the old flow-matrix-only model would have produced.
default_region_spec <- function(flows) {
  parties <- rownames(flows)
  out <- list()
  for (p in parties) out[[p]] <- default_subblock_for(p, flows)
  out
}

#' Build the full default sub-blocks-by-region object
default_subblocks_by_region <- function(flows_by_region) {
  lapply(flows_by_region, default_region_spec)
}

#' Normalise shares within an origin's sub-block data frame
normalise_shares <- function(spec) {
  if (is.null(spec) || nrow(spec) == 0) return(spec)
  s <- sum(spec$share)
  if (s == 0) {
    spec$share <- rep(1 / nrow(spec), nrow(spec))
  } else {
    spec$share <- spec$share / s
  }
  spec
}

#' Add a new empty sub-block row to an origin's spec
add_subblock_row <- function(spec, parties, origin) {
  default_rank <- sort(setdiff(parties, origin))
  new_row <- data.frame(share = 0, stringsAsFactors = FALSE)
  new_row$ranking <- list(default_rank)
  if (is.null(spec) || nrow(spec) == 0) return(new_row)
  rbind(spec, new_row)
}

#' Remove a sub-block row by index
remove_subblock_row <- function(spec, i) {
  if (is.null(spec) || nrow(spec) <= 1) {
    # Always keep at least one row.
    return(spec)
  }
  spec[-i, , drop = FALSE]
}

#' Convert a ranking from "comma,separated,string" to vector and back
ranking_to_string <- function(r) paste(r, collapse = ", ")
ranking_from_string <- function(s) {
  parts <- trimws(strsplit(s, ",")[[1]])
  parts[nzchar(parts)]
}
