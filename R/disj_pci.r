#' @title Multiple scaling in a disjoint manner using positive items
#'
#' @description \code{disj_pci} is an internal function. It returns a list of
#'   data frames. In order to do that, \code{disj_pci} sets up an empty list of
#'   scales (\code{scls}) and scaling can begin: When a first scale is set up in
#'   there (1) \code{disj_pci} tries to find the two items with the highest
#'   positive correlation in the data frame. (2) If this correlation is greater
#'   than the pre-specified lower bound (\code{rit_min}), the two items are
#'   summed up to build the core of the upcoming scale. Therefore the core is
#'   nothing more than the sum score of these two items. After the building
#'   process the core items are excluded from the data frame. They are now part
#'   of the first scale. (4) While the (positive) correlation of any other of
#'   the remaining items in the data frame (with that sum score) is greater than
#'   the pre-specified lower bound (\code{rit_min}) \code{disj_pci} enlarges the
#'   scale with the appropriate item. (5 = 1) If there are at least two items
#'   left in the data frame, yielding a correlation coefficient greater than the
#'   set lower bound, \code{disj_pci} restart the scaling process with a new
#'   scale.
#'
#' @param df a data frame object.
#'
#' @param rit_min a numerical constant to specify the (corrected item total)
#'   correlation. The value of this lower bound must in the range of 0 to 1. The
#'   default is set to \code{.3}.
#'
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{"pairwise.complete.obs"}.
#'
#' @details \code{disj_pci} includes only those items in the scaling process
#'   yielding a positive correlation, e.g. with the sum  score. It implements
#'   the main idea behind the multiple scaling approach and is therefore the
#'   basic building block for its expansions -- including disjoint and
#'   overlapping scaling processes allowing to take negative correlations into
#'   account. \code{disj_pci} is called internally when \code{disjoint} is used.
#'   Each data frame in the list of data frames is (once again) a genuine subset
#'   of the specified one.
#'
#'   The \code{use} argument takes control over the treatment of missing values
#'   when correlation matrices are build. In a scaling process this happens at
#'   least twice: first when determining the core (the two items of the
#'   correlation matrix with the highest linear relationship), and second when
#'   an item is considered to be part of this scale.
#'
#'   Hint: If there ought to be two items in the scaling process, having an
#'   equal correlation, e.g. with the sum score, always the first one is used.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.

disj_pci <- function(df, rit_min, use) {
  scls <- list()
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    # use = use: make sure its an ARG
    cormat <- cor(df, use = use)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < rit_min) break
    # Take the first(!) maximum
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    scls[[scls_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      scls_len <- length(scls)
      cormat <- cor(rowSums(scls[[scls_len]]), df, use = use)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < rit_min) break
      fstmaxp <- which(cormat == maxcor)
      scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
      df <- df[-fstmaxp]
      }
    }
  scls
}
