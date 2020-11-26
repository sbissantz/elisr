test_that("non-df's are detected", {
  mat <- as.matrix(iris)
  expect_error(
    disjoint(df = mat),
    "`df` is not a data.frame."
  )
})
