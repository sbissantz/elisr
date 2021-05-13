test_that("msdf inherits", {
  msdf <- disjoint(trust)
  expect_true(is.msdf(msdf))
})
