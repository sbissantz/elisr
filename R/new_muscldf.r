#' @title Creator Function to Build A New \code{muscldf}
#'
#' @description A \code{muscldf} is a multiple scaled data frame. A multiple
#' scaled data frame is nothing more than a list of scales with attributes. A
#' scale is just a subset of the specified data frame (e.g., \code{disjoint}) or
#' an extension of the given \code{muscldf} (e.g., \code{overlap} -- were each
#' scale is built according to the crystallization principle.
#'
#' @details The \code{muscldf} is only for internal use. A check function
#' assures that the argument to \code{overlap} is of that type. Another useful
#' feature is that the attributes provide information which settings were used
#' in the scaling process -- after scaling has been done. So one can be sure
#' that all parameters are set correctly.

new_muscldf <- function(x = list(), method, rit_min,
                        # sclvals must be set NULL -- assures that
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
  scls_nms <- nme_muscldf(x)
  # Should do the trick
  if (is.null(sclvals)) {
    sclvals <- (attr(sclvals, "sclvals"))
  if (is.null(sclvals)) {
    sclvals <- match.call()$sclsvals
  }
  }
  structure(x, class = "muscldf", method = method, rit_min = rit_min,
            negative_too = negative_too, sclvals = sclvals, df = df,
            names = scls_nms)
}
