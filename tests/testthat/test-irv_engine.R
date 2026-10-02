# test-irv_engine.R -----------------------------------------------------
# Run with: testthat::test_dir("tests/testthat")
# ----------------------------------------------------------------------

library(testthat)
source("../../R/irv_engine.R")

# Helper to build a square flow matrix from a named list of named vectors.
make_flows <- function(spec) {
  parties <- names(spec)
  m <- matrix(0, length(parties), length(parties),
              dimnames = list(parties, parties))
  for (p in parties) {
    row <- spec[[p]]
    m[p, names(row)] <- row
  }
  m
}

test_that("majority winner on first round needs no transfers", {
  votes <- c(Lab = 600, Con = 300, LD = 100)
  flows <- make_flows(list(
    Lab = c(LD = 0.7, Con = 0.3),
    Con = c(LD = 0.6, Lab = 0.4),
    LD  = c(Lab = 0.8, Con = 0.2)
  ))
  res <- irv_count(votes, flows)
  expect_equal(res$winner, "Lab")
  expect_equal(nrow(res$rounds), 1)
  expect_length(res$eliminated, 0)
})

test_that("two-party runoff: lowest is eliminated and transfers", {
  votes <- c(Lab = 400, Con = 400, LD = 200)
  flows <- make_flows(list(
    Lab = c(LD = 0.9, Con = 0.1),
    Con = c(LD = 0.5, Lab = 0.5),
    LD  = c(Lab = 0.9, Con = 0.1)  # LD voters break heavily for Lab
  ))
  res <- irv_count(votes, flows)
  expect_equal(res$winner, "Lab")
  expect_equal(res$eliminated, "LD")
  # Lab should have 600 in round 2, Con 400.
  expect_equal(res$rounds[2, "Lab"], 600)
  expect_equal(res$rounds[2, "Con"], 400)
})

test_that("ties in elimination are broken alphabetically (deterministic)", {
  votes <- c(A = 500, B = 100, C = 100, D = 300)
  flows <- make_flows(list(
    A = c(D = 1, B = 0.5, C = 0.5),
    B = c(A = 1, C = 0.5, D = 0.5),
    C = c(A = 1, B = 0.5, D = 0.5),
    D = c(A = 1, B = 0.5, C = 0.5)
  ))
  res <- irv_count(votes, flows)
  # B and C tie at 100; alphabetical order means B is eliminated first.
  expect_equal(res$eliminated[1], "B")
})

test_that("multi-round count reaches a strict majority", {
  votes <- c(Lab = 350, Con = 300, LD = 200, Grn = 100, Ref = 50)
  flows <- make_flows(list(
    Lab = c(LD = 0.7, Grn = 0.2, Con = 0.05, Ref = 0.05),
    Con = c(Ref = 0.6, LD = 0.3, Lab = 0.05, Grn = 0.05),
    LD  = c(Lab = 0.6, Grn = 0.3, Con = 0.05, Ref = 0.05),
    Grn = c(Lab = 0.7, LD  = 0.25, Con = 0.025, Ref = 0.025),
    Ref = c(Con = 0.8, Lab = 0.1, LD  = 0.05, Grn = 0.05)
  ))
  res <- irv_count(votes, flows)
  expect_true(res$winner %in% c("Lab", "Con"))
  # Final round leader must exceed 500 (strict majority of 1000).
  final <- res$rounds[nrow(res$rounds), names(votes)]
  expect_true(max(final, na.rm = TRUE) > 500)
})

test_that("eliminated parties show NA in subsequent rounds", {
  votes <- c(A = 400, B = 350, C = 250)
  flows <- make_flows(list(
    A = c(B = 0.5, C = 0.5),
    B = c(A = 0.5, C = 0.5),
    C = c(A = 1, B = 0)  # C transfers entirely to A
  ))
  res <- irv_count(votes, flows)
  expect_equal(res$winner, "A")
  expect_true(is.na(res$rounds[2, "C"]))
})

test_that("total votes are conserved across rounds", {
  votes <- c(W = 200, X = 180, Y = 160, Z = 140)
  flows <- make_flows(list(
    W = c(X = 0.4, Y = 0.4, Z = 0.2),
    X = c(W = 0.4, Y = 0.3, Z = 0.3),
    Y = c(W = 0.5, X = 0.3, Z = 0.2),
    Z = c(W = 0.3, X = 0.3, Y = 0.4)
  ))
  res <- irv_count(votes, flows)
  total <- sum(votes)
  for (i in seq_len(nrow(res$rounds))) {
    expect_equal(sum(res$rounds[i, names(votes)], na.rm = TRUE), total)
  }
})

test_that("ranking_from_flow handles zero-weight entries", {
  flow_row <- c(A = 0.5, B = 0, C = 0.3, D = 0.2)
  r <- ranking_from_flow("origin", flow_row)
  expect_equal(r, c("A", "C", "D"))  # B excluded
})
