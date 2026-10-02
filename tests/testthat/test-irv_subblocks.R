# test-irv_subblocks.R --------------------------------------------------
library(testthat)
source("../../R/irv_engine.R")

test_that("irv_count_blocks reproduces simple deterministic case", {
  # 400 A, 400 B, 200 C. No majority. C eliminated, all C voters rank
  # B 2nd, so B gets 600 and wins.
  blocks <- list(
    list(ranking = c("A", "B", "C"), count = 400),
    list(ranking = c("B", "A", "C"), count = 400),
    list(ranking = c("C", "B", "A"), count = 200)
  )
  res <- irv_count_blocks(blocks, parties = c("A", "B", "C"))
  expect_equal(res$winner, "B")
  expect_equal(res$eliminated, "C")
})

test_that("sub-blocks within a single origin party transfer differently", {
  # 1000 Lab voters split 60/40: 60% rank Green 2nd, 40% rank Con 2nd.
  # 400 Con, 200 Green. Lab leads on 1st prefs but no one over 50%.
  # Round 1: Lab 1000, Con 400, Green 200. Green eliminated.
  # Green voters all rank Lab 2nd. Lab gets 200 -> Lab=1200, Con=400.
  # Lab over 50% wins.
  blocks <- list(
    list(ranking = c("Lab", "Green", "Con"), count = 600),
    list(ranking = c("Lab", "Con", "Green"), count = 400),
    list(ranking = c("Con", "Lab", "Green"), count = 400),
    list(ranking = c("Green", "Lab", "Con"), count = 200)
  )
  res <- irv_count_blocks(blocks, parties = c("Lab", "Con", "Green"))
  expect_equal(res$winner, "Lab")
})

test_that("heterogeneous sub-blocks change the outcome vs deterministic", {
  # Tight three-way: A=400, B=350, C=250.
  # Deterministic model: C voters all break for B -> B=600 wins.
  # Heterogeneous model: half of C breaks for A, half for B
  # -> A=525, B=475 -> A wins.
  parties <- c("A", "B", "C")

  # Deterministic (all C voters go to B):
  blocks_det <- list(
    list(ranking = c("A", "B", "C"), count = 400),
    list(ranking = c("B", "A", "C"), count = 350),
    list(ranking = c("C", "B", "A"), count = 250)
  )
  res_det <- irv_count_blocks(blocks_det, parties)
  expect_equal(res_det$winner, "B")

  # Heterogeneous (C splits 50/50 between A and B):
  blocks_het <- list(
    list(ranking = c("A", "B", "C"), count = 400),
    list(ranking = c("B", "A", "C"), count = 350),
    list(ranking = c("C", "A", "B"), count = 125),
    list(ranking = c("C", "B", "A"), count = 125)
  )
  res_het <- irv_count_blocks(blocks_het, parties)
  expect_equal(res_het$winner, "A")
})

test_that("build_blocks_from_subblocks pads unranked parties alphabetically", {
  votes <- c(Lab = 1000, Con = 500, LD = 300, Green = 200)
  spec <- data.frame(share = c(0.7, 0.3))
  spec$ranking <- list(c("Green"), c("LD", "Con"))  # partial rankings
  subs <- list(Lab = spec)
  blocks <- build_blocks_from_subblocks(votes, subs)
  lab_blocks <- Filter(function(b) b$ranking[1] == "Lab", blocks)
  expect_equal(length(lab_blocks), 2)
  # First block: Lab, Green, then unranked alphabetical: Con, LD
  expect_equal(lab_blocks[[1]]$ranking, c("Lab", "Green", "Con", "LD"))
  # Second block: Lab, LD, Con, then unranked: Green
  expect_equal(lab_blocks[[2]]$ranking, c("Lab", "LD", "Con", "Green"))
})

test_that("shares are normalised even when they don't sum to 1", {
  votes <- c(A = 1000, B = 100)
  spec <- data.frame(share = c(2, 8))  # 20% / 80% after normalisation
  spec$ranking <- list(c("B"), c("B"))
  subs <- list(A = spec)
  blocks <- build_blocks_from_subblocks(votes, subs)
  counts <- vapply(Filter(function(b) b$ranking[1] == "A", blocks),
                   `[[`, numeric(1), "count")
  expect_equal(sort(counts), c(200, 800))
})

test_that("missing sub-block falls back to default flows", {
  votes <- c(A = 500, B = 300, C = 200)
  flows <- matrix(0, 3, 3, dimnames = list(c("A","B","C"), c("A","B","C")))
  flows["A", "C"] <- 0.8; flows["A", "B"] <- 0.2  # A prefers C heavily
  flows["B", "A"] <- 1; flows["C", "A"] <- 1
  # No sub-block specified for any party -> uses default_flows.
  blocks <- build_blocks_from_subblocks(votes, subblocks = list(),
                                         default_flows = flows)
  a_block <- Filter(function(b) b$ranking[1] == "A", blocks)[[1]]
  expect_equal(a_block$ranking, c("A", "C", "B"))  # weight order
})
