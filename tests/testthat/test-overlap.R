test_that("positive scaling", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- overlap(disjoint(trust, mrit_min = .55),
                   mrit_min = .3)
   expect_length(msdf, 2)
   expect_length(msdf$scl_1, 13)
   expect_length(msdf$scl_2, 13)
})

test_that("attributes inherits", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- overlap(disjoint(trust, mrit_min = .55, negative_too = TRUE,
                      sclvals = c(1, 7)), negative_too = TRUE)
   expect_equal(attr(msdf, "mrit_min"), .55)
   expect_setequal(attr(msdf, "sclvals"), c(1, 7))
})

test_that("core option", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- overlap(disjoint(trust, mrit_min = .55),
                     mrit_min = .3, overlap_with = "core")
   eupalmnt <- trust[, "eupalmnt"]
   expect_length(msdf, 2)
   expect_length(msdf$scl_1, 13)
   expect_length(msdf$scl_2, 13)
})

test_that("overlap_with metod", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- disjoint(trust)
   expect_error(overlap(msdf, overlap_with = "test_1"))
   expect_error(overlap(msdf, negative_too = TRUE, sclvals = c(1, 7),
                        overlap_with = "test_2"))
})

test_that("reversed items", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- overlap(disjoint(trust, mrit_min = .55))
   msdf_rev <- overlap(disjoint(within(trust, fedgovt <- 8 - fedgovt),
                                mrit_min = .55), negative_too = TRUE,
                                sclvals = c(1, 7))
   expect_setequal(msdf$scl_1[, "eupalmnt"], msdf_rev$scl_1[, "eupalmnt"])
   expect_setequal(msdf$scl_1[, "fedgovt"], msdf_rev$scl_1[, "fedgovt"])
})

test_that("core option + reversed items", {
   skip_if_not(capabilities("long.double"),
               message = "noLD -- skip. Denied to increase tolerance.")
   msdf <- overlap(
      disjoint(
         within(trust, {eupalmnt <- 8 - eupalmnt; fedgovt <- 8 - fedgovt}),
         mrit_min = .55), mrit_min = .3, overlap_with = "core",
      negative_too = TRUE, sclvals = c(1, 7))
   expect_length(msdf, 3)
   expect_length(msdf$scl_1, 13)
   expect_length(msdf$scl_2, 13)
   expect_length(msdf$scl_3, 13)
   expect_named(msdf$scl_1, colnames(trust), ignore.order = TRUE)
})
