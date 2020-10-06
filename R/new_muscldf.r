#'   This so called \code{muscldf} is nothing more
#'   than a list of scales (in this case genuine subsets of that data frame).
#'
# Constructor -------------------------------------------------------------
# TODO overlap_with not as Attrb?

new_muscldf <- function(x = list(), method, rit_min,
                        # sclvals must be set NULL -- asures
                        negative_too, sclvals = NULL,
                        df) {
  stopifnot(exprs = {
    is.list(x)
    is.double(rit_min) || is.null(rit_min)
    is.logical(negative_too)
    is.vector(sclvals) || is.null(sclvals)
    is.data.frame(eval(df, parent.frame()))
  })
  method <- match.arg(method, c("disjoint", "overlap"))
  # Should do the trick
  if (is.null(sclvals)) {
    sclvals <- (attr(sclvals, "sclvals"))
  if (is.null(sclvals)) {
    sclvals <- match.call()$sclsvals
  }
  }
  structure(x, class = "muscldf", method = method, rit_min = rit_min,
            negative_too = negative_too, sclvals = sclvals, df = df)
}
