#' @title Multiple Scaling In A Disjoint Manner Using Negative Items Too
#'
#' @description \code{disj_pci} is an internal function. It returns a list of
#'   data frames. In order to do that, \code{disj_nci} sets up an empty list of
#'   scales (\code{scls}) and scaling can begin: When a first scale is set up in
#'   there (1) \code{disj_nci} tries to find the two items with the highest
#'   correlation within the data frame. (2) If the absolute value of this
#'   correlation is greater than the pre-specified lower bound (\code{rit_min}),
#'   the two items are summed together to build the core of the upcoming scale.
#'   Therefore. the core is nothing more than the sum score of these two items.
#'   For that reason, if an item has a negative correlation with the core of the
#'   scale it needs to be reversed. After the building process the core items
#'   are excluded from the data frame. They are now part of the first scale. (4)
#'   While the correlation (i.e., the absolute value of this correlation) of any
#'   of the remaining items in the data frame (with that sum score) is greater
#'   than the pre-specified lower bound, \code{disj_pci} enlarges the scale with
#'   the appropriate item. (5 = 1) If there are at least two items left in the
#'   data frame, yielding in addition a correlation coefficient greater than the
#'   set lower bound, \code{disj_pci} restart the scaling process with a new
#'   scale.
#'
#' @param df a data frame object.
#'
#' @param rit_min a numerical constant to specify the (corrected item total)
#'   correlation. The value of this lower bound must in the range of 0 to 1. The
#'   default is set to \code{.3}.
#'
#' @param negative_too a logical constant indicating whether items with a
#'   negative correlation should be included in the scaling process. The default
#'   is set to \code{FALSE}.
#'
#' @param sclvals a numerical vector of length 2 indicating the first and the
#'   full scale value. Consider using the shape \code{c(min,max)}.
#'
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{"pairwise.complete.obs"}.
#'
#' @details \code{disj_nci} is an internal function and an extension of
#'   \code{disj_pci}. {disj_nci} allows for the use of items having a negative
#'   correlation, e.g. with the sum score in the scaling process. To be more
#'   precise, \code{rvrs_var} takes the responsibility for reversing a variable,
#'   which is necessary to enable the inclusion of these items. \code{disj_nci}
#'   is called internally when \code{disjoint} is used. Each data frame in the
#'   list of data frames is (once again) a genuine subset of the specified one.
#'
#'   The \code{use} argument takes control over the treatment of missing values
#'   when correlation matrices are build. In a scaling process this happens at
#'   least twice: first when determining the core (the two items of the
#'   correlation matrix with the highest linear relationship), and second when
#'   an item is considered to be part of this scale.
#'
#'   Hint: If there ought to be two items in the scaling process, having an
#'   equal correlation, e.g. with the sum score, always the first one is used.

disj_nci <- function(df, rit_min, sclvals, use) {
 scls <- list()
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    # use = use: make sure it's an ARG
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
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < rit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
        }else{
          var_rev <- rvrs_var(var = df[fstmaxp], sclvals)
          # message("`", names(df[fstmaxp]), "` was recoded")
          scls[[scls_len]] <- cbind(scls[[scls_len]], var_rev)
          }
      df <- df[-fstmaxp]
    }
  }
  scls
}
