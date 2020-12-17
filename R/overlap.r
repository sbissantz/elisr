#' @title Multiple Scaling In An Overlapping Manner
#'
#' @description \code{overlap} returns a multiple, overlapping scaled version of
#'   the specified \code{muscldf}. It is then often explored using the eponymous
#'   function (\code{\link{explore}}).
#'
#' @param muscldf a multiple scaled data frame (built with \code{disjoint}).
#'
#' @param rit_min a numerical constant to set the (corrected item total)
#'   correlation. The value of this lower bound must be in the range of 0 to 1.
#'   If no value is entered (\code{NULL}, the default) \code{overlap} tries to
#'   get the one specified in \code{disjoint}, by looking for a \code{rit_min}
#'   attribute in the given \code{muscldf}.
#'
#' @param negative_too a logical constant indicating whether items with a
#'   negative correlation should be included in the scaling process. The default
#'   is set to \code{FALSE}.
#'
#' @param overlap_with a string telling \code{overlap} which items to start the
#'   scaling process with. One can choose to use either the "core" of each scale
#'   or a "fragment". The default is set to "fragment".
#'
#' @param sclvals a numerical vector of length 2 indicating the first and the
#'   full scale value. Consider using the shape \code{c(min,max)}.
#'
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @details The \code{use} argument takes control over the treatment of missing
#'   values when correlation matrices are build. In a scaling process this
#'   happens at least twice: first when determining the core (the two items of
#'   the correlation matrix with the highest linear relationship), and second
#'   when an item is considered to be part of this scale.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.
#'
#' @examples
#' # Build a muscldf
#' muscldf <- disjoint(mtcars, rit_min = .4)
#'
#' # Using positive correlations (and `pairwise.complete.obs`)
#' overlap(muscldf, rit_min = .6, overlap_with = "core")
#'
#' # Including negative correlations (and `pairwise.complete.obs`)
#' overlap(muscldf, rit_min = .7, negative_too = TRUE, sclvals = c(-3,3))
#'
#' # Changing the treatment of missing values
#' overlap(muscldf, rit_min = .6, use = "all.obs")
#'

#' @export
overlap <- function(muscldf, rit_min = NULL, negative_too = FALSE,
                    overlap_with = "fragment", sclvals = NULL,
                    use = "pairwise.complete.obs") {
  check_neg(negative_too)
  check_muscldf(muscldf)
  if (is.null(rit_min)) {
    rit_min <- attr(muscldf, "rit_min")
    }
  check_rit(rit_min)
  check_ovlp(overlap_with)
  if (negative_too) {
    sclvals_attr <- attr(muscldf, "sclvals")
    compare_sclvals(sclvals, sclvals_attr)
    if (is.null(sclvals)) {
      sclvals <- sclvals_attr
    }
    check_sclvals(sclvals)
    scls <- ovlp_nci(muscldf, rit_min, overlap_with, sclvals, use = use)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = TRUE,
                sclvals = sclvals)
  }else{
    scls <- ovlp_pci(muscldf, rit_min, overlap_with, use = use)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = FALSE)
  }
}
