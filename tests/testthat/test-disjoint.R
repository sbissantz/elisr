test_that("positive scaling", {
   msdf <- disjoint(trust, mrit_min = .55)
   expect_length(msdf, 2)
   expect_length(msdf$scl_1, 7)
   expect_length(msdf$scl_2, 2)
})

test_that("negative scaling", {
   msdf <- disjoint(trust, mrit_min = .55)
   msdf_rev <- disjoint(
      within(trust, polpati <- 8 - polpati),
      mrit_min = .55, negative_too = TRUE, sclvals = c(1, 7))
   expect_setequal(msdf$scl_1[, "polpati"],  msdf_rev$scl_1[, "polpati"])
})
