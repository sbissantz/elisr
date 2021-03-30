#' @title Creator Function to Build A New \code{msdf}
#'
#' @description A \code{msdf} is a multiple scaled data frame. A multiple
#' scaled data frame is nothing more than a list of scales with attributes. A
#' scale is just a subset of the specified data frame (e.g., \code{disjoint}) or
#' an extension of the given \code{msdf} (e.g., \code{overlap} -- were each
#' scale is built according to the crystallization principle.
#'
#' @details The \code{msdf} is only for internal use. A check function
#' assures that the argument to \code{overlap} is of that type. Another useful
#' feature is that the attributes provide information which settings were used
#' in the scaling process -- after scaling has been done. So one can be sure
#' that all parameters are set correctly.

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
