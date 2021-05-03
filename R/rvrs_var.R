#' @title Reverse a variable
#'
#' @description \code{rvrs_var()} reverses an item using the specified scaling
#'   values. It handles the following types of scales:
#'   \itemize{
#'   \item  ...-3 -2 -1 0 1 2 3..., e.g., \code{sclvals = c(-3, 3)}
#'   \item  0 1 2 3 4 5 6..., e.g.,  \code{sclvals = c(0, 7)}
#'   \item  1 2 3 4 5 6 7..., e.g., \code{sclcals = c(1, 7)}
#'   }
#'
#' @param var a variable or item (often a column from a data frame).
#'
#' @param sclvals the start and end point of a scale (specify: \code{c(sp,ep)})
#'
#' @return A reversed numeric vector.
#'
rvrs_var <- function(var, sclvals) {
  # ... -3 -2 -1 0 1 2 3 ...
  if (sclvals[1] < 0) {
    return(var * -1)
  }
  # 0 1 2 3 4 5 6 ...
  if (sclvals[1] == 0 && sclvals[2] > 0) {
    return(sclvals[2] - var)
  }
  # 1 2 3 4 5 6 7
  if (sclvals[1] == 1 && sclvals[2] > 0) {
    return((sclvals[2] + 1) - var)
  }
}
