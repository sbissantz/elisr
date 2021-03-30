#' @title Multiple Scaling In An Overlapping Manner
#'
#' @description \code{overlap} returns a multiple, overlapping scaled version of
#'   the specified \code{msdf}.
#'
#' @param msdf a multiple scaled data frame (built with \code{disjoint}).
#'
#' @param mrit_min a numerical constant of length 1 to specify the marginal
#'   corrected item-total correlation. It's value is in the range of 0-1. The
#'   default is set to \code{.3}.
#'
#' @param negative_too a logical constant indicating whether reversed items
#'   should be included. The default is set to \code{FALSE}.

#' @param overlap_with a string telling \code{overlap} the items it should
#'   extend on (in each particular case). To build up on all variables of a
#'   fragment use \code{fragment}, for the cores-only option type \code{core}.
#'   The default is set to "fragment".
#'
#' @param sclvals a numerical vector of length 2 indicating the start and
#'   endpoint of a scale. Use something like \code{c(min,max)}.
#'
#' @param use an optional string to specify how missing values will enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{pairwise.complete.obs}.
#'
#' @detail \code{use} clarifies how to set up a correlation matrix in the
#'   presence of missing values. In a scaling process this happens at least
#'   twice. First, when determining the core items (the two items in the
#'   correlation matrix with the highest linear relationship). Second, when an
#'   item is proposed for an emerging scale.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.
#'
#' @examples
#' # Build a msdf
#' msdf <- disjoint(mtcars, mrit_min = .4)
#'
#' # Using positive correlations (and `pairwise.complete.obs`)
#' overlap(msdf, mrit_min = .6, overlap_with = "core")
#'
#' # Including negative correlations (and `pairwise.complete.obs`)
#' overlap(msdf, mrit_min = .7, negative_too = TRUE, sclvals = c(-3,3))
#'
#' # Changing the treatment of missing values
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
