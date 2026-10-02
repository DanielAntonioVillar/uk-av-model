# irv_engine.R ----------------------------------------------------------
# Instant-Runoff Voting engine for single-constituency contests.
#
# Two APIs:
#
#   irv_count_blocks(blocks, parties)
#     A "block" is a homogeneous group of ballots sharing one complete
#     ranking:
#       block = list(ranking = c("Lab", "Green", "LD", ...),
#                    count   = 12345)
#     Multiple blocks can share a 1st-preference party — this is how
#     within-party heterogeneity is expressed.
#
#   irv_count(votes, flows)
#     Backward-compatible wrapper. Takes a named vote vector and a
#     flow matrix, derives one deterministic ranking per origin party,
#     and calls irv_count_blocks.
#
# Full preferences are enforced: every block ranks every party that's
# standing, so no votes exhaust.
# ----------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Derive a deterministic ranking from a flow row (legacy helper)
#' @keywords internal
ranking_from_flow <- function(origin, flow_row) {
  others <- flow_row[names(flow_row) != origin]
  others <- others[others > 0]
  if (length(others) == 0) return(character(0))
  ord <- order(-others, names(others))
  names(others)[ord]
}

#' Current top-of-ranking for a block among still-active parties
#' @keywords internal
block_top <- function(block, active) {
  hit <- block$ranking[block$ranking %in% active]
  if (length(hit) == 0) NA_character_ else hit[1]
}

#' Tally votes by current top, given a list of blocks
#' @keywords internal
tally_blocks <- function(blocks, active) {
  out <- setNames(rep(0, length(active)), active)
  for (b in blocks) {
    p <- block_top(b, active)
    if (!is.na(p)) out[p] <- out[p] + b$count
  }
  out
}

#' Run an IRV count from a list of ballot blocks
#'
#' @param blocks List of blocks, each a list with elements:
#'   - ranking: character vector of party names in preference order
#'   - count:   numeric, number of ballots in this block
#' @param parties Character vector of all parties standing. If NULL,
#'   derived from the union of party names in any block's ranking.
#' @param majority Fraction needed to win (default 0.5).
#'
#' @return list(winner, rounds, eliminated, final_margin)
irv_count_blocks <- function(blocks, parties = NULL, majority = 0.5) {
  stopifnot(is.list(blocks), length(blocks) > 0)
  if (is.null(parties)) {
    parties <- unique(unlist(lapply(blocks, `[[`, "ranking")))
  }
  stopifnot(is.character(parties), length(parties) >= 1)

  blocks <- Filter(function(b) any(b$ranking %in% parties), blocks)
  stopifnot(length(blocks) > 0)

  total <- sum(vapply(blocks, function(b) b$count, numeric(1)))
  threshold <- total * majority
  active <- parties

  rounds <- list()
  eliminated <- character(0)
  winner <- NA_character_
  runner_up <- 0
  top <- 0

  repeat {
    tally <- tally_blocks(blocks, active)
    rounds[[length(rounds) + 1L]] <- tally

    top <- max(tally)
    leader <- names(tally)[which.max(tally)]

    if (top > threshold || length(active) == 1L) {
      winner <- leader
      runner_up <- if (length(tally) > 1) sort(tally, decreasing = TRUE)[2] else 0
      break
    }

    lowest <- min(tally)
    losers <- names(tally)[tally == lowest]
    loser <- sort(losers)[1]
    eliminated <- c(eliminated, loser)
    active <- setdiff(active, loser)
  }

  rounds_df <- do.call(rbind, lapply(rounds, function(r) {
    row <- setNames(rep(NA_real_, length(parties)), parties)
    row[names(r)] <- r
    row
  }))
  rounds_df <- as.data.frame(rounds_df)
  rounds_df$round <- seq_len(nrow(rounds_df))
  rounds_df <- rounds_df[, c("round", parties)]

  list(
    winner       = winner,
    rounds       = rounds_df,
    eliminated   = eliminated,
    final_margin = unname(top - runner_up)
  )
}

#' Backward-compatible API: vote vector + flow matrix
irv_count <- function(votes, flows, majority = 0.5) {
  stopifnot(is.numeric(votes), !is.null(names(votes)), all(votes >= 0))
  stopifnot(is.matrix(flows), nrow(flows) == ncol(flows))
  stopifnot(identical(rownames(flows), colnames(flows)))

  votes <- votes[votes > 0]
  parties <- names(votes)
  stopifnot(all(parties %in% rownames(flows)))
  flows_sub <- flows[parties, parties, drop = FALSE]

  blocks <- lapply(parties, function(p) {
    rest <- ranking_from_flow(p, flows_sub[p, ])
    if (length(rest) == 0) rest <- sort(setdiff(parties, p))
    list(ranking = c(p, rest), count = unname(votes[p]))
  })
  irv_count_blocks(blocks, parties = parties, majority = majority)
}

#' Build ballot blocks from a sub-block specification
#'
#' @param votes Named numeric vector: 1st-pref vote counts per party.
#' @param subblocks Named list. Names = origin parties; values =
#'   data.frame with columns:
#'     - share:   numeric, fraction of that party's voters (sums to ~1)
#'     - ranking: list-column of character vectors — the ranking from
#'                2nd preference onward (origin party = 1st, implicit)
#'   Parties without a sub-block entry fall back to a single block
#'   derived from default_flows[origin, ].
#' @param default_flows Optional flow matrix for fallback ranking.
build_blocks_from_subblocks <- function(votes, subblocks, default_flows = NULL) {
  parties <- names(votes)
  blocks <- list()
  for (origin in parties) {
    n_voters <- unname(votes[origin])
    if (n_voters <= 0) next
    spec <- subblocks[[origin]]
    if (is.null(spec) || nrow(spec) == 0) {
      if (!is.null(default_flows) && origin %in% rownames(default_flows)) {
        keep <- intersect(parties, colnames(default_flows))
        rest <- ranking_from_flow(origin, default_flows[origin, keep])
      } else {
        rest <- character(0)
      }
      if (length(rest) == 0) rest <- sort(setdiff(parties, origin))
      blocks[[length(blocks) + 1L]] <- list(
        ranking = c(origin, rest), count = n_voters
      )
      next
    }
    shares <- spec$share
    if (sum(shares) == 0) {
      shares <- rep(1 / nrow(spec), nrow(spec))
    } else {
      shares <- shares / sum(shares)
    }
    for (i in seq_len(nrow(spec))) {
      r <- spec$ranking[[i]]
      if (is.character(r) && length(r) == 1) {
        r <- trimws(strsplit(r, ",")[[1]])
        r <- r[nzchar(r)]
      }
      r <- r[r %in% parties & r != origin]
      # Full-preferential: pad with any unranked parties alphabetically.
      missing <- sort(setdiff(parties, c(origin, r)))
      full_ranking <- c(origin, r, missing)
      blocks[[length(blocks) + 1L]] <- list(
        ranking = full_ranking,
        count   = n_voters * shares[i]
      )
    }
  }
  blocks
}

#' Run IRV across many constituencies (sub-block aware)
irv_count_all <- function(results_df, region_lookup,
                          subblocks_by_region = NULL,
                          default_flows_by_region = NULL) {

  stopifnot(all(c("constituency_id", "party", "votes") %in% names(results_df)))
  stopifnot(all(c("constituency_id", "region") %in% names(region_lookup)))

  results_df$region <- NULL
  merged <- merge(results_df, region_lookup, by = "constituency_id")
  split_results <- split(merged, merged$constituency_id)

  out <- lapply(split_results, function(con) {
    region <- con$region[1]
    v <- setNames(con$votes, con$party)
    sb <- if (!is.null(subblocks_by_region)) subblocks_by_region[[region]] else NULL
    df <- if (!is.null(default_flows_by_region)) default_flows_by_region[[region]] else NULL
    if (!is.null(df)) {
      keep <- intersect(names(v), rownames(df))
      df <- df[keep, keep, drop = FALSE]
      v <- v[keep]
    }
    if (length(v) == 0) return(NULL)
    blocks <- build_blocks_from_subblocks(v, subblocks = sb %||% list(),
                                          default_flows = df)
    res <- irv_count_blocks(blocks, parties = names(v))
    data.frame(
      constituency_id = con$constituency_id[1],
      winner          = res$winner,
      rounds_taken    = nrow(res$rounds),
      final_margin    = res$final_margin,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Run IRV for a single constituency and return full round-by-round detail
#'
#' Used by the UI to re-run a count when the user clicks on a specific
#' constituency. Returns the rich `irv_count_blocks` result including the
#' per-round tally data frame and the elimination order.
irv_count_one <- function(results_df, region_lookup, constituency_id,
                          subblocks_by_region = NULL,
                          default_flows_by_region = NULL) {
  con <- results_df[results_df$constituency_id == constituency_id, ]
  if (nrow(con) == 0) return(NULL)
  region <- region_lookup$region[region_lookup$constituency_id == constituency_id][1]
  v <- setNames(con$votes, con$party)
  sb <- if (!is.null(subblocks_by_region)) subblocks_by_region[[region]] else NULL
  df <- if (!is.null(default_flows_by_region)) default_flows_by_region[[region]] else NULL
  if (!is.null(df)) {
    keep <- intersect(names(v), rownames(df))
    df <- df[keep, keep, drop = FALSE]
    v <- v[keep]
  }
  if (length(v) == 0) return(NULL)
  blocks <- build_blocks_from_subblocks(v, subblocks = sb %||% list(),
                                        default_flows = df)
  irv_count_blocks(blocks, parties = names(v))
}
