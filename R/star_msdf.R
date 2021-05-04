#' @title Create and test for a multiple scaled data frame
#'
#' @param x either a multiple scaled data frame (\code{new_msdf()}) or an
#'   arbitrary object to test (\code{is_msdf()}).
#'
#' @param method the method used to produce the multiple scaled data frame
#'   (either \code{disjoint()} or \code{overlap()}).
#'
#' @param mrit_min a numeric constant of length one to specify the marginal
#'   corrected item-total correlation. Its value is in the range of 0-1.
#'
#' @param sclvals a numeric vector of length two indicating the start- and
#'   endpoint of a scale.
#'
#' @param negative_too a logical constant indicating whether reversed items are
#'   included in the analysis. The default is set to \code{FALSE}.
#'
#' @param df the data frame to analyze.
#'
#' @details Objects of type `msdf` are for internal use only.
#'
#' @name msdf
#'
NULL

#' @rdname msdf
#'
#' @description \code{new_msdf()} creates a multiple scaled data frame
#'   (\code{msdf}).
#'
#' @returns \code{new_msdf()} returns a list of data frames with a few
#'   attributes that partially summarize the scaling process.
#
new_msdf <- function(x = list(), method, mrit_min,
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

#' @rdname msdf
#'
#' @description \code{is.msdf()} tests if an object is a multiple scaled data
#'   frame.
#'
#' @returns \code{is.msdf()} returns a logical vector of length one. \code{TRUE}
#'   indicates that the object is of type \code{msdf}.
#'
#'@export
is.msdf <- function(x) inherits(x, "msdf")
