#' @title Multiple scaling in a disjoint manner using negative items too
#'
#' @description \code{disj_nci} is an internal function and returns a list of
#'   data frames. In addition, \code{disj_nci} sets up an empty list of
#'   scales (\code{scls}) and scaling begins: (1) A (first) scale is set up.
#'   (2) \code{disj_nci} tries to find the two items with the highest
#'   correlation in the data frame. (3) If the absolute value of this
#'   correlation is greater than the pre-specified lower bound (\code{mrit_min}),
#'   \code{disj_nci} adds up the two items to build the core of the emerging
#'   scale. In that process, any negative correlation will be reversed. After
#'   developing the core both items will be excluded from the data frame. They
#'   are now part of the first scale. (4) While the absolute value of the
#'   correlation between the sum score and a remaining items in the data frame
#'   is greater than the pre-specified lower bound, \code{disj_nci} enlarges the
#'   scale with the appropriate item. (5 = 1) If there are at least two items
#'   left in the data frame, yielding a correlation coefficient greater than the
#'   set lower bound, \code{disj_nci} starts over again.
#'
#' @param df a data frame object.
#'
#' @param mrit_min a numerical constant to specify the marginal corrected item
#'   total correlation. The value must be in the range of 0-1.
#'
#' @param sclvals a numerical vector of length 2 indicating the start and
#'   endpoint of a scale.
#'
#' @param use an optional string to specify how missing values will enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @details \code{disj_nci} is an internal function which extends the scope of
#'   \code{disj_pci}. {disj_nci} allows the presence of negative items (i.e., a
#'   negative correlation between item and sum score).
#'
#'   \code{use} clarifies how to set up a correlation matrix in the presence of
#'   missing values. In a typical scaling process this happens at least twice.
#'   First, when determining the core items (the two items in the correlation
#'   matrix with the highest linear relationship). Second, when an item is
#'   proposed for an emerging scale.

#'   @importFrom stats cor
disj_nci <- function(df, mrit_min, sclvals, use) {
 scls <- list()
 msg <- list() ###
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    # use = use: make sure it's an ARG
    cormat <- cor(df, use = use)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < mrit_min) break
    # Take the first(!) maximum
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    scls[[scls_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      scls_len <- length(scls)
      cormat <- cor(rowSums(scls[[scls_len]]), df, use = use)
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < mrit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
        }else{
          var_rev <- rvrs_var(var = df[fstmaxp], sclvals)
          msg <- c(msg, names(var_rev)) ###
          scls[[scls_len]] <- cbind(scls[[scls_len]], var_rev)
          }#
      df <- df[-fstmaxp]
    }
  }
 rvrs_note(msg, applicant = "disjoint")
 scls
}
