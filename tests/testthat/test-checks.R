test_that("df is a data frame", {
  mat <- as.matrix(iris[, -5])
  expect_error(check_df(x = mat))
})

test_that("df has more than 1 column", {
  df_l1 <- as.data.frame(iris[, 1])
  expect_error(check_df(x = df_l1))
})

test_that("df has colnames", {
  df_nn <- iris[, -5]
  colnames(df_nn) <- NULL
  expect_error(check_df(x = df_nn))
})

test_that("df has no NA colnames", {
  df_na <- iris[, -5]
  colnames(df_na) <- c("a", "b", "c", NA)
  expect_warning(check_df(x = df_na))
})

test_that("df has unique colnames", {
  df_dn <- iris[, -5]
  colnames(df_dn) <- c("a", "b", "c", "c")
  expect_warning(check_df(x = df_dn))
})

test_that("sclval is a two element vector", {
  expect_error(check_sclvals(x = 1:3))
})

test_that("sclval has the `c(min,max)` shape", {
  expect_error(check_sclvals(x = c(10, 5)))
})

test_that("sclval vector (1st & 2nd time) match", {
  expect_error(compare_sclvals(x = c(10, 4), x_attr = c(10, 5)))
})

test_that("rit is a double & of length 1", {
  expect_error(check_rit(x = "abc"))
})

test_that("rit is in the rangge [0;1]", {
  expect_error(check_rit(x = 1.2))
})

test_that("overlap_with is a character vector of length 1", {
  expect_error(check_ovlp(x = 1))
  expect_error(check_ovlp(x = c("a", "b")))
})

test_that("msdf is one of its type", {
  expect_error(check_msdf(x = as.data.frame(1:10)))
})

test_that("negative_too", {
  expect_error(check_neg(x = 1))
  expect_error(check_neg(x = c(TRUE, FALSE)))
})

test_that("compatibility of mrit_min and max cor", {
  df <- iris[, -5]
  expect_error(check_comp(x = df, mrit_min = 10, use = "pairwise.complete.obs"))
})

test_that("mrit_min", {
  expect_error(check_mrit(as.integer(1)))
  expect_error(check_mrit(c(TRUE, FALSE)))
  expect_error(check_mrit(1.5))
  expect_warning(check_mrit(0))
})
