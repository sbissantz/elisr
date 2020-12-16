context("Test disjoint")

test_that("multiplication works", {
  msdf <- disjoint(trust, rit_min = .3)
  expect_equal(length(msdf[[1]]), 13)
})





