#' @title Multiple Scaling In An Overlapping Manner Using Positive Items

#' @description \code{ovlp_nci} is an internal function and returns a list of
#'   data frames. It takes a disjoint (\code{msdf}) and tries to extend it with
#'   those items in the data frame which are not yet built into the fragment
#'   (aka., counterpart). Despite the default, one can extend on the cores of
#'   each fragment. The underlying expansion algorithm remains the same. The
#'   only difference is that \code{ovlp_nci} applies this algorithm to each
#'   fragment within `msdf`. The overlapping family of functions is thus a
#'   multiple extension of the disjoint scaling approach to various fragments.
#'
#' @param msdf a multiple scaled data frame (built with \code{disjoint}).
#'
#' @param mrit_min a numerical constant of length 1 to specify the marginal
#'   corrected item-total correlation. It's value is in the range of 0-1.
#'
#' @param overlap_with a string telling \code{overlap} the items it should
#'   extend on (in each particular case). To build up on all variables of a
#'   fragment use \code{fragment}, for the cores-only option type \code{core}.
#'
#' @param use an optional string to specify how missing values will enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @details \code{use} clarifies how to set up a correlation matrix in the
#'   presence of missing values. In a scaling process this happens at least
#'   twice. First, when determining the core items (the two items in the
#'   correlation matrix with the highest linear relationship). Second, when an
#'   item is proposed for an emerging scale.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.

#'   @importFrom stats cor
ovlp_pci <- function(msdf, mrit_min, overlap_with, use) {

# Function ----------------------------------------------------------------

one_ovlp_pci <- function(scl, ebscl, mrit_min, use) {
   while (ncol(ebscl) >= 1) {
     cormat <- cor(rowSums(scl), ebscl, use = use)
     maxcor <- max(cormat[cormat < 1])
     if (maxcor < mrit_min) break
     fstmaxp <- which(cormat == maxcor)
     scl <- cbind(scl, ebscl[fstmaxp])
     ebscl <- ebscl[-fstmaxp]
     }
   scl
 }

# Pre-Set's ---------------------------------------------------------------

df <- eval(attr(msdf, "df"))
# scl_nms <- lapply(msdf, names)
# core_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# Procedure & Options -----------------------------------------------------

switch(overlap_with,
       fragment = {
         # scls <- msdf
         # scls <- lapply(scl_nms, extr_itms, df = df)
         scl_nms <- lapply(msdf, names)
         ebscls <- lapply(scl_nms, extreb_itms, df = df)
         },
       core = {
          msdf <- lapply(msdf, extr_core)
          core_nms <- lapply(msdf, extr_core_nms)
          ebscls <- lapply(core_nms, extreb_itms, df = df)
         # scls <- lapply(core_nms, extr_itms, df = df)
         # ebscls <- lapply(core_nms, extreb_itms, df = df)
         },
       stop("Unknown overlapping method. Use either `fragment` or `core`",
            call. = FALSE)
       )
# if ( ncol(ebscls) == 0) stop ("No items to overlap. Consider lowering mrit_min")
Map(one_ovlp_pci, msdf, ebscls, MoreArgs = list(mrit_min, use))
# Map(one_ovlp_pci, scls, ebscls, MoreArgs = list(mrit_min, use))
}
