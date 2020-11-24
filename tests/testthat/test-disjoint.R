context("Disjoint scaling")
library(musclr)

test_that("inheritance works", {
  muscldf_disj <- disjoint(df = iris[,-length(iris)], rit_min = .3,
                           negative_too = FALSE)
  expect_is(muscldf_disj, "muscldf")
})
test_that("input validation is correct", {
  expect_error(disjoint(df = iris[,-length(iris)], rit_min = .3,
                           negative_too = TRUE),
               "`sclvals` is not a two element vector.")
})

