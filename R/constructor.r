# TODO Ordering of Arguments

# Constructor -------------------------------------------------------------

new_muscldf <- function(x = list(), df = quote(), method = "disjoint",
                        rit_min = double(), negative_too = FALSE,
                        sclvals = NULL) {
  stopifnot(exprs = {
    is.list(x)
    is.data.frame(df)
    is.double(rit_min)
    is.logical(negative_too)
    is.vector(sclvals) || is.null(sclvals)
    })
  method <- match.arg(method, c("disjoint", "overlap"))
  df <- match.call()$df

  structure(x, class = "muscldf", method = method, rit_min = rit_min,
            negative_too = negative_too, sclvals = sclvals, df = df)
}
