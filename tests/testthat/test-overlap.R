test_that("positive scaling", {
   msdf_d <- disjoint(trust, mrit_min = .55)
   (msdf_o <- overlap(msdf_d, mrit_min = .3))
   eupalmnt <- trust[, "eupalmnt"]
   expect_equal(length(msdf_o), 2)
   expect_equal(length(msdf_o$scl_1), 13)
   expect_equal(length(msdf_o$scl_2), 13)
   expect_equal(msdf_o$scl_1[, 1], eupalmnt)
})

test_that("attributes inherits", {
   msdf_d <- disjoint(trust, mrit_min = .55, negative_too = TRUE,
                      sclvals = c(1, 7))
   msdf_o <- overlap(msdf_d, negative_too = TRUE)
   expect_equal(attr(msdf_o, "mrit_min"), .55)
   expect_equal(attr(msdf_o, "sclvals"), c(1, 7))
})

test_that("core option", {
   msdf_d <- disjoint(trust, mrit_min = .55)
   msdf_o <- overlap(msdf_d, mrit_min = .3, overlap_with = "core")
   eupalmnt <- trust[, "eupalmnt"]
   expect_equal(length(msdf_o), 2)
   expect_equal(length(msdf_o$scl_1), 13)
   expect_equal(length(msdf_o$scl_2), 13)
   expect_equal(msdf_o$scl_1[, 1], eupalmnt)
})

test_that("overlap_with metod", {
   msdf_d <- disjoint(trust)
   expect_error(overlap(msdf_d, overlap_with = "me"))
   expect_error(overlap(msdf_d, negative_too = TRUE, sclvals = c(1, 7),
                        overlap_with = "me"))
})

test_that("reversed items", {
   mdf <- disjoint(trust, mrit_min = .55)
   msdf <- overlap(mdf, mrit_min = .55)
   trust$fedgovt <- (1 + 7) - trust[, "fedgovt"]
   mdf_new <- disjoint(trust, mrit_min = .55)
   msdf_new <- overlap(mdf_new, mrit_min = .3, negative_too = TRUE,
                       sclvals = c(1, 7))
   expect_equal(msdf$scl_1[, "eupalmnt"], msdf_new$scl_1[, "eupalmnt"])
   expect_equal(msdf$scl_1[, "fedgovt"], msdf_new$scl_1[, "fedgovt"])
})

test_that("core option + reversed items", {
   trust$eupalmnt <- (1 + 7) - trust[, "eupalmnt"]
   trust$fedgovt <- (1 + 7) - trust[, "fedgovt"]
   msdf_d <- disjoint(trust, mrit_min = .55)
   msdf_o <- overlap(msdf_d, mrit_min = .3, overlap_with = "core",
                      negative_too = TRUE, sclvals = c(1, 7))
   expect_equal(length(msdf_o), 3)
   expect_equal(length(msdf_o$scl_1), 13)
   expect_equal(length(msdf_o$scl_2), 13)
   expect_equal(length(msdf_o$scl_3), 13)
   expect_equal(colnames(msdf_o$scl_1[1]), "newsppr")
})
