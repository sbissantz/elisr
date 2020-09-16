# TODO Ordering of Arguments

# Constructor -------------------------------------------------------------

new_muscldf <- function(x = list(), method = "disjoint",
                        rit_min = NULL, negative_too = FALSE,
                        sclvals = NULL, df = data.frame()) {
  stopifnot(exprs = {
    is.list(x)
    is.data.frame(eval(df))
    is.double(rit_min) || is.null(rit_min)
    is.logical(negative_too)
    is.vector(eval(sclvals)) || is.null(eval(sclvals))
    })
  method <- match.arg(method, c("disjoint", "overlap"))
  structure(x, class = "muscldf", method = method, rit_min = rit_min,
            negative_too = negative_too, sclvals = sclvals, df = df)
}
