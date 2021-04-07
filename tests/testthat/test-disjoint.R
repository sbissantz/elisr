test_that("positive scaling", {
   msdf <- disjoint(trust, mrit_min = .55)
   eupalmnt <- trust[, "eupalmnt"]
   expect_equal(length(msdf), 2)
   expect_equal(length(msdf$scl_1), 7)
   expect_equal(length(msdf$scl_2), 2)
   expect_equal(msdf$scl_1[, 1], eupalmnt)
})

test_that("negative scaling", {
   msdf <- disjoint(trust, mrit_min = .55)
   msdf_new <- disjoint(df = {
     trust$polpati <- (1 + 7) - trust[, "polpati"]
     trust
   }, mrit_min = .55, negative_too = TRUE, sclvals = c(1, 7))
   expect_equal(msdf$scl_1[, "polpati"], msdf_new$scl_1[, "polpati"])
})
