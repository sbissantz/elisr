test_that("name msdf's", {
  res_nme <- nme_msdf(2)
  expect_equal(res_nme, "scl_1")
})

test_that("calculate average correlation", {
  x <- c(1, 2, 3, 4, 5, 6, 7)
  y <- c(3, 4, 5, 6, 7, 1, 2)
  z <- c(5, 6, 7, 1, 2, 3, 4)
  mat <- cbind(x, y, z)
  res_rbar <- calc_rbar(mat, use = "pairwise.complete.obs")
  expect_equal(round(res_rbar, digits = 2), -0.33)
})

test_that("calculate cronbach's alpha", {
  msdf_disj <- disjoint(iris[, -5])
  res_alpha <- calc_alpha(msdf_disj[[1]], use = "pairwise.complete.obs")
  expect_equal(round(res_alpha, digits = 2), 0.96)
})

test_that("calculate corrected item total correlation", {
  msdf_disj <- disjoint(iris[, -5])
  res_rit <- calc_rit(msdf_disj[[1]], use = "pairwise.complete.obs")
  expect_equal(round(res_rit[[1]], digits = 2), 0.86)
})

test_that("extracting the core items", {
  msdf_disj <- disjoint(iris[, -5])
  res_extr <- extr_core(msdf_disj$scl_1)
  expect_equal(iris[, c("Petal.Width", "Petal.Length"), drop = FALSE], res_extr)
})

test_that("extracting everyting but specified items", {
  res_extreb <- extreb_itms(iris, "Sepal.Length")
  expect_equal(iris[, -1], res_extreb)
})

test_that("reversing variables", {
  one_seven <- 1:7 ; res_one_seven <- rvrs_var(one_seven, c(1, 7))
  expect_equal(res_one_seven, 7:1)
  o_seven <- 0:7 ; res_o_seven <- rvrs_var(o_seven, c(0, 7))
  expect_equal(res_o_seven, 7:0)
  two_two <- -2:2 ; res_two_two <- rvrs_var(two_two, c(-2, 2))
  expect_equal(res_two_two, 2:-2)
})

test_that("reversing message", {
  expect_message(disjoint(trust, negative_too = TRUE, sclvals = c(1, 7)))
})
