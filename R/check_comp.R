#' @title Test if any correlation is greater than a specified value
#'
#' @description \code{check_comp()} is an internal function. \code{check_comp()}
#'   examines the correlation matrix, \code{cor(df)}. It complains (throws an
#'   error) if no correlation in \code{cor(df)} is greater than the specified
#'   \code{mrit_min}.
#'
#' @param x some arbitrary input to be checked.
#'
#' @param mrit_min a numeric constant of length 1 to specify the marginal
#'   corrected item-total correlation.
#'
#' @param use an optional string to specify how missing values enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_comp <- function(x, mrit_min, use) {
  cormat <- cor(x, use = use)
  maxcor <- max(cormat[cormat < 1])
  if (maxcor < mrit_min)
    stop("`mrit_min` is smaller than any correlation found in `df`.",
         call. = FALSE)
}
