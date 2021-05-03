#' @title Test the appropriateness of an input
#'
#' @description \code{check_df()} is an internal function which is called for
#'   its side effects. It guarantees that \code{x} is an appropriate data frame
#'   for the analysis. That mean: It verifies that \code{x} has less than two
#'   variables (a single item can't build a core), \code{x} has column names
#'   (used to pre-build \code{scls} in the overlapping process), if the column
#'   names are unique, and not of type \code{NA}. It throws an error if any of
#'   these requirements are not met. Additionally, it warns the user if
#'   the provided \code{colnames} are not unique or \code{NA}.
#'
#' @param x some arbitrary input to be checked.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_df <- function(x) {
  if (!is.data.frame(x))
    stop("`df` is not a data.frame.", call. = FALSE)
  if (!length(x) >= 2)
    stop("`df` has less than 2 variables.")
  x_nms <- names(x)
  if (is.null(x_nms))
    stop("`df` has no (col)names.",
         call. = FALSE)
  x_unms <- unique(x_nms)
  if (!identical(x_nms, x_unms))
    warning("`df` (col)names are not unique.", call. = FALSE)
  if (anyNA(x_nms))
    warning("`df` has (col)names of type `NA`.", call. = FALSE)
}
