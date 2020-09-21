# TODO Ordering of Arguments
# TODO overlapping method either `cores` or `full_scl`. Use match.arg()

# Constructor -------------------------------------------------------------

new_muscldf <- function(x = list(), method = "disjoint", rit_min = NULL,
                        negative_too = FALSE, sclvals = NULL, df = NULL) {
  stopifnot(exprs = {
    is.list(x)
    is.double(rit_min) || is.null(rit_min)
    is.logical(negative_too)
    is.vector(sclvals) || is.null(sclvals)
    is.data.frame(eval(df, parent.frame()))
  })
  method <- match.arg(method, c("disjoint", "overlap"))
  structure(x, class = "muscldf", method = method, rit_min = rit_min,
            negative_too = negative_too, sclvals = sclvals, df = df)
}
