test_that("positive scaling", {
   msdf_d <- disjoint(trust, mrit_min = .55)
   (msdf_o <- overlap(msdf_d, mrit_min = .3))
   eupalmnt <- trust[,"eupalmnt"]
   expect_equal(length(msdf_o), 2)
   expect_equal(length(msdf_o$scl_1), 13)
   expect_equal(length(msdf_o$scl_2), 13)
   expect_equal(msdf_o$scl_1[,1], eupalmnt)
})

test_that("negative scaling", {
   mdf <- disjoint(trust, mrit_min = .55)
   msdf <- overlap(mdf, mrit_min = .3)
   trust$eupalmnt <- (1 + 7) - trust[,"eupalmnt"]
   mdf_new <- disjoint(trust, mrit_min = .55)
   msdf_new <- overlap(mdf_new, mrit_min = .3, negative_too = TRUE,
                       sclvals = c(1,7))
   expect_equal(msdf$scl_1[,"eupalmnt"], msdf_new$scl_1[,"eupalmnt"])
})
