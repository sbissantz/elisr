context("Test disjoint")
# Hint: Test with allbus item
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



test_that("name muscldf's", {
  res_nme <- nme_muscldf(2)
  expect_equal(res_nme, "scl_1")
})

