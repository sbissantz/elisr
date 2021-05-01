#' @title Creator function to build a new multiple scaled data frame
#'
#' @description \code{msdf} is a multiple scaled data frame. An object of that
#'   type is nothing more than a list of data frames with some additional
#'   attributes. The data frame itself is a subset of the given data set --
#'   arranged according to the crystallization principle.
#'
#' @param x a multiple scaled data frame.
#'
#' @param method the method used to produce the multiple scaled data frame
#'   (either disjoint or overlap).
#'
#' @param mrit_min a numeric constant of length 1 to specify the marginal
#'   corrected item-total correlation. Its value is in the range of 0-1.
#'
#' @param sclvals a numeric vector of length 2 indicating the start- and
#'   endpoint of a scale.
#'
#' @param negative_too a logical constant indicating whether reversed items are
#'   included in the analysis. The default is set to \code{FALSE}.
#'
#' @param df the data frame to analyze.
#'
#' @details Multiple scaled data frames are only for internal use. A test
#'   function ensures that the specified object is a multiple scaled data frame.

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
