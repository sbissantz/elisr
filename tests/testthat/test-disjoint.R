test_that("positive scaling", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- disjoint(trust, mrit_min = .55)
   len <- vapply(msdf, length, FUN.VALUE = integer(1))
   expect_setequal(len, c(7L, 2L))
})

test_that("negative scaling", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- disjoint(trust, mrit_min = .55)
   msdf_rev <- disjoint(
      within(trust, polpati <- 8 - polpati),
      mrit_min = .55, negative_too = TRUE, sclvals = c(1, 7))
   expect_setequal(msdf$scl_1[, "polpati"],  msdf_rev$scl_1[, "polpati"])
})
