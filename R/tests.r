
# Disjoint ----------------------------------------------------------------
# ...positive items only
#(msdf_1 <- disjoint(mtcars,
#                    rit_min = .1))
#msdf_2 <- disjoint(mtcars,
#                   rit_min = .1,
#                   negative_too = TRUE)
## Error? Korrekt
#(msdf_2 <- disjoint(mtcars,
#                    rit_min = .1,
#                    negative_too = TRUE,
#                    sclvals = c(1,10)))
#
## Overlap -----------------------------------------------------------------
#
#(msdf_3 <- overlap(msdf_1, rit_min = NULL))
#
# (msdf_4 <- overlap(msdf_1, rit_min = NULL, negative_too = TRUE))
# Error? Korrect
# (msdf_4 <- overlap(msdf_2, rit_min = NULL, negative_too = TRUE))
# Error? Korrekt
# (msdf_5 <- overlap(msdf_2, rit_min = NULL, negative_too = TRUE , sclvals = c(1,5)))
# No Error? Check
# (msdf_5 <- overlap(msdf_2, rit_min = NULL, negative_too = TRUE , sclvals = c(1,10)))
#
