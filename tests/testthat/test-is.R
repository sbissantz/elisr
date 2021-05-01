test_that("msdf inherits", {
  msdf <- disjoint(iris[-5])
  expect_equal(is.msdf(msdf), TRUE)
})
