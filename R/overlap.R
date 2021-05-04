#' @title Multiple scaling -- the overlapping way
#'
#' @description \code{overlap()} returns a overlapped version (either extended,
#'   or reversed, or both) of the specified \code{msdf}.
#'
#' @param msdf a multiple scaled data frame (built with \code{disjoint()}).
#'
#' @param mrit_min a numeric constant of length 1 to specify the marginal
#'   corrected item-total correlation. Its value is in the range of 0-1. The
#'   default is set to \code{.3}.
#'
#' @param negative_too a logical constant indicating whether reversed items are
#'   included in the analysis. The default is set to \code{FALSE}.
#'
#' @param overlap_with a string telling \code{overlap()} the set of items for
#'   the extension. To build up on all variables of a fragment use
#'   \code{fragment}, for the core-only option type \code{core}. The default is
#'   set to "fragment".
#'
#' @param sclvals a numeric vector of length 2 indicating the start- and
#'   endpoint of a scale. Use something like \code{c(min,max)}.
#'
#' @param use an optional string to specify how missing values enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{pairwise.complete.obs}.
#'
#' @details \code{use} clarifies how to set up a correlation matrix in the
#'   presence of missing values. In a typical scaling process this happens at
#'   least twice. First, when determining the core items (the two items in the
#'   correlation matrix with the highest linear relationship). Second, when an
#'   item is proposed for an emerging scale.
#'
#'   Note that \code{overlap()} uses \code{\link[stats]{cor}}'s default
#'   method \code{pearson}.
#'
#' @return A multiple scaled data frame (\code{msdf}).
#'
#' @references Müller-Schneider, T. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie, 30(4), 305-315.
#'   https://doi.org/10.1515/zfsoz-2001-0404
#'
#' @examples
#' # Build a msdf
#' msdf <- disjoint(mtcars, mrit_min = .4)
#'
#' # Use positive correlations (extend on fragments)
#' overlap(msdf, mrit_min = .6)
#'
#' # Use positive correlations (extend on cores)
#' overlap(msdf, mrit_min = .6, overlap_with = "core")
#'
#' # Include negative correlations
#' overlap(msdf, mrit_min = .7, negative_too = TRUE, sclvals = c(-3,3))
#'
#' # Change the treatment of missing values
#' overlap(msdf, mrit_min = .6, use = "all.obs")
#'

#' @export
overlap <- function(msdf, mrit_min = NULL, negative_too = FALSE,
                    overlap_with = "fragment", sclvals = NULL,
                    use = "pairwise.complete.obs") {
  check_neg(negative_too)
  check_msdf(msdf)
  if (is.null(mrit_min)) {
    mrit_min <- attr(msdf, "mrit_min")
    }
  check_mrit(mrit_min)
  check_ovlp(overlap_with)
  if (negative_too) {
    sclvals_attr <- attr(msdf, "sclvals")
    compare_sclvals(sclvals, sclvals_attr)
    if (is.null(sclvals)) {
      sclvals <- sclvals_attr
    }
    check_sclvals(sclvals)
    scls <- ovlp_nci(msdf, mrit_min, overlap_with, sclvals, use = use)
    new_msdf(scls, df = attr(msdf, "df"), method = "overlap",
                mrit_min = mrit_min, negative_too = TRUE,
                sclvals = sclvals)
  }else{
    scls <- ovlp_pci(msdf, mrit_min, overlap_with, use = use)
    new_msdf(scls, df = attr(msdf, "df"), method = "overlap",
                mrit_min = mrit_min, negative_too = FALSE)
  }
}
