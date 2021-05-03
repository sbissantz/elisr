#' @title Multiple scaling in a disjoint manner using negative items too
#'
#' @description \code{disj_nci()} is an internal function. It is a loop which
#'   runs through the following steps: (1) Set up a (first) scale. (2) find the
#'   two items with the highest positive correlation in the data set. (3) If the
#'   absolute value of this correlation is greater than the pre-specified lower
#'   bound (\code{mrit_min}), add up the two items to build the core of the
#'   emerging scale. (4) While the absolute value of the correlation between the
#'   sum score and a remaining items in the data frame is greater than
#'   \code{mrit_min}, flavor the scale with the appropriate item. If necessary,
#'   reverse the item. (5) If there are at least two leftovers in the data
#'   frame, that meet the inclusion criterion, start over again.
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
#' @details \code{disj_nci()} is an internal function which extends the scope of
#'   \code{disj_pci()}. {disj_nci()} allows to reverse negative items.
#'
#'   \code{use} clarifies how to set up a correlation matrix in the presence of
#'   missing values. In a typical scaling process this happens at least twice.
#'   First, when determining the core items (the two items in the correlation
#'   matrix with the highest linear relationship). Second, when an item is
#'   proposed for an emerging scale.
#'
#' @return A list of data frames. It results from applying the above algorithm.
#'
#' @references Müller-Schneider, T. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie, 30(4), 305-315.
#'   https://doi.org/10.1515/zfsoz-2001-0404

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
