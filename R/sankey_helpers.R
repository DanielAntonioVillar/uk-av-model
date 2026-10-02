# sankey_helpers.R -----------------------------------------------------
# Convert an irv_count_blocks() result into the node/link data frames
# that networkD3::sankeyNetwork expects.
#
# Structure:
#   - One column per round. Each column lists every party still active
#     in that round.
#   - Nodes are named "<Party>_R<round>", e.g. "Lab_R1", "Lab_R2".
#   - Links between consecutive rounds:
#       * Surviving parties carry their own votes forward (e.g. Lab_R1
#         -> Lab_R2 = round-1 Lab tally).
#       * Eliminated parties' votes split out into the next-round
#         tallies of whoever picked them up.
# ----------------------------------------------------------------------

#' Build Sankey nodes + links from an IRV result and the blocks used
#'
#' @param res Result of irv_count_blocks().
#' @param blocks The ballot blocks fed into irv_count_blocks(). Needed
#'   to compute round-to-round transfer volumes.
#' @param parties Character vector of all parties standing.
#'
#' @return list(nodes = data.frame(name), links = data.frame(source,
#'   target, value)). Indices in source/target are zero-based, matching
#'   networkD3's convention.
sankey_from_irv <- function(res, blocks, parties) {
  rounds_df <- res$rounds
  n_rounds <- nrow(rounds_df)
  # Active parties per round = those with non-NA tally that round.
  active_per_round <- lapply(seq_len(n_rounds), function(r) {
    p <- names(rounds_df)[-1]  # drop "round" column
    p[!is.na(rounds_df[r, p])]
  })

  # Build node table. Each node is a (party, round) pair.
  node_rows <- list()
  for (r in seq_len(n_rounds)) {
    for (p in active_per_round[[r]]) {
      node_rows[[length(node_rows) + 1L]] <- data.frame(
        name  = paste0(p, "_R", r),
        party = p,
        round = r,
        stringsAsFactors = FALSE
      )
    }
  }
  nodes <- do.call(rbind, node_rows)
  nodes$id <- seq_len(nrow(nodes)) - 1L   # zero-based for D3

  # Helper: which block currently sits with which party in a given round?
  #
  # We simulate the count again here, walking elimination by elimination,
  # to determine for each block where it sat in round r and where it
  # moved to in round r+1. That's the same logic as the engine but we
  # need to observe it.
  active <- parties
  block_state <- lapply(blocks, function(b) {
    list(ranking = b$ranking, count = b$count,
         current = b$ranking[b$ranking %in% active][1])
  })

  links <- data.frame(source = integer(0), target = integer(0),
                      value = numeric(0))
  node_id <- function(party, round) {
    nodes$id[nodes$party == party & nodes$round == round]
  }

  for (r in seq_len(n_rounds - 1L)) {
    # Who got eliminated between round r and round r+1?
    in_r   <- active_per_round[[r]]
    in_rp1 <- active_per_round[[r + 1L]]
    eliminated_this_round <- setdiff(in_r, in_rp1)

    # For every block, record (current_party_r) -> (current_party_r+1).
    # Aggregate by that pair to get the link weight.
    flow <- list()
    for (i in seq_along(block_state)) {
      from <- block_state[[i]]$current
      # Update: if `from` is being eliminated, find next active in
      # ranking after removing eliminated parties.
      if (from %in% eliminated_this_round) {
        rem <- setdiff(block_state[[i]]$ranking, eliminated_this_round)
        rem <- rem[rem %in% in_rp1]
        block_state[[i]]$current <- if (length(rem)) rem[1] else NA
      }
      to <- block_state[[i]]$current
      if (is.na(from) || is.na(to)) next
      key <- paste(from, to, sep = "->")
      flow[[key]] <- (flow[[key]] %||% 0) + block_state[[i]]$count
    }

    # Convert aggregated flow into link rows.
    for (key in names(flow)) {
      parts <- strsplit(key, "->", fixed = TRUE)[[1]]
      from <- parts[1]; to <- parts[2]
      links <- rbind(links, data.frame(
        source = node_id(from, r),
        target = node_id(to, r + 1L),
        value  = flow[[key]],
        from_party = from,
        to_party = to,
        stringsAsFactors = FALSE
      ))
    }

    # Now actually remove eliminated parties from active for next iter.
    active <- setdiff(active, eliminated_this_round)
  }

  list(nodes = nodes, links = links)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
