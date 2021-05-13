test_that("printing works", {
 skip_if_not(capabilities("long.double"),
             message = "noLD -- skip. Denied to increase tolerance.")
 msdf <- disjoint(trust)
 p_msdf <- print(msdf)
 expect_equal(round(p_msdf$scl_1[1], 2), 0.92)
})
