# vote_share_helpers.R -------------------------------------------------
# Vote-share editing: lets the user override the regional vote share
# for each party. The actual constituency-level votes are then rescaled
# so the regional average matches the target, while preserving the
# within-region variation of the underlying data.
#
# Algorithm (per region, per party p):
#   1. Compute the current regional vote share: total p votes / total votes.
#   2. Multiplier = target_p / current_p.
#   3. For each constituency, multiply party p's vote count by the
#      multiplier.
#   4. Renormalise each constituency so its parties sum to the
#      constituency's original total votes (preserving turnout).
#
# This is essentially proportional swing applied uniformly within
# region, which is the standard psephological tool.
# ----------------------------------------------------------------------

#' Compute the actual regional vote shares from the data
#'
#' @param results_df Long data frame with constituency_id, region,
#'   party, votes.
#' @return Named list keyed by region. Each entry is a named numeric
#'   vector of vote shares (summing to 1) keyed by party.
compute_regional_shares <- function(results_df) {
  agg <- aggregate(votes ~ region + party, data = results_df, FUN = sum)
  out <- list()
  for (r in unique(agg$region)) {
    sub <- agg[agg$region == r, ]
    total <- sum(sub$votes)
    if (total == 0) {
      shares <- setNames(rep(0, nrow(sub)), sub$party)
    } else {
      shares <- setNames(sub$votes / total, sub$party)
    }
    out[[r]] <- shares
  }
  out
}

#' Apply user-edited regional shares to the constituency-level data
#'
#' For each region/party pair:
#'   actual_share = sum(party votes in region) / sum(all votes in region)
#'   multiplier   = target_share / actual_share
#'   votes_new    = votes_old * multiplier (per constituency)
#' Then within each constituency, renormalise so total votes equal the
#' original constituency total (preserves turnout).
#'
#' @param results_df Long data frame (constituency_id, region, party, votes).
#' @param target_shares Named list (region -> named numeric vector of
#'   target shares per party). Targets within a region need not sum to
#'   1 — they will be normalised.
#' @return Modified results_df with `votes` rescaled.
apply_regional_shares <- function(results_df, target_shares, max_iter = 5L,
                                  tol = 1e-4) {
  con_totals <- aggregate(votes ~ constituency_id, data = results_df, FUN = sum)
  con_totals <- setNames(con_totals$votes, con_totals$constituency_id)

  rescaled <- results_df
  # Normalise targets first so they sum to 1 within each region.
  norm_targets <- lapply(target_shares, function(tgt) {
    if (sum(tgt) == 0) return(tgt)
    tgt / sum(tgt)
  })

  # Iterate: rescale, renormalise, check, repeat. Converges in 2-3 passes.
  for (iter in seq_len(max_iter)) {
    baseline <- compute_regional_shares(rescaled)

    for (r in names(norm_targets)) {
      tgt <- norm_targets[[r]]
      if (sum(tgt) == 0) next
      base <- baseline[[r]]
      in_region <- rescaled$region == r
      for (p in names(tgt)) {
        base_share <- if (p %in% names(base)) unname(base[p]) else 0
        if (base_share == 0) next
        mult <- tgt[[p]] / base_share
        mask <- in_region & rescaled$party == p
        rescaled$votes[mask] <- rescaled$votes[mask] * mult
      }
    }

    # Renormalise each constituency to its original turnout.
    new_totals <- aggregate(votes ~ constituency_id, data = rescaled, FUN = sum)
    new_totals <- setNames(new_totals$votes, new_totals$constituency_id)
    scale <- con_totals / new_totals
    scale[!is.finite(scale)] <- 1
    rescaled$votes <- rescaled$votes * scale[rescaled$constituency_id]

    # Check convergence on regional shares.
    new_baseline <- compute_regional_shares(rescaled)
    max_err <- 0
    for (r in names(norm_targets)) {
      tgt <- norm_targets[[r]]
      if (sum(tgt) == 0) next
      base <- new_baseline[[r]]
      common <- intersect(names(tgt), names(base))
      max_err <- max(max_err, max(abs(tgt[common] - base[common])))
    }
    if (max_err < tol) break
  }

  rescaled$votes <- round(rescaled$votes)
  rescaled <- rescaled[rescaled$votes > 0, ]
  rescaled
}

#' Are the given target shares effectively unchanged from the baseline?
#'
#' Useful to short-circuit the rescaling if the user hasn't touched the
#' vote-share editor.
shares_are_default <- function(target_shares, baseline, tol = 1e-4) {
  if (length(target_shares) == 0) return(TRUE)
  for (r in names(target_shares)) {
    tgt <- target_shares[[r]]
    if (sum(tgt) == 0) next
    tgt <- tgt / sum(tgt)
    base <- baseline[[r]]
    common <- intersect(names(tgt), names(base))
    if (any(abs(tgt[common] - base[common]) > tol)) return(FALSE)
  }
  TRUE
}
