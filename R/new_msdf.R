#' @title Creator function to build a new \code{msdf}
#'
#' @description A \code{msdf} is a multiple scaled data frame. An object of that
#'   type is nothing more than a list of data frames with some attributes
#'   attached to it. The data frame itself is a subset of the given data set --
#'   arranged according to the crystallization principle.
#'
#' @details Multiple scaled data frames are only for internal use. A test
#'   function ensures that the argument to \code{overlap} is a \code{msdf}.

new_msdf <- function(x = list(), method, mrit_min,
                        # sclvals must be set NULL -- assures that
                        negative_too, sclvals = NULL,
                        df) {
  stopifnot(exprs = {
    is.list(x)
    is.double(mrit_min) || is.null(mrit_min)
    is.logical(negative_too)
    is.vector(sclvals) || is.null(sclvals)
    is.data.frame(eval(df, parent.frame()))
  })
  method <- match.arg(method, c("disjoint", "overlap"))
  scls_nms <- nme_msdf(x)
  # Should do the trick
  if (is.null(sclvals)) {
    sclvals <- (attr(sclvals, "sclvals"))
  if (is.null(sclvals)) {
    sclvals <- match.call()$sclsvals
  }
  }
  structure(x, class = "msdf", method = method, mrit_min = mrit_min,
            negative_too = negative_too, sclvals = sclvals, df = df,
            names = scls_nms)
}
