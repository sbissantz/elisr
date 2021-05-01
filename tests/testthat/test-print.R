test_that("multiplication works", {
 msdf <- disjoint(trust)
 p_msdf <- print.msdf(msdf)
 expect_equal(round(p_msdf$scl_1[1], 2), 0.92)
})
