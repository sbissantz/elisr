#' @title Test the appropriateness of an input
#'
#' @description \code{check_neg()} is an internal function which is called for
#'   its side effects. It throws an error if the (\code{negative_too} input is
#' not a logical constant of length 1 and/or a missing value.
#'
#' @details To verify both, that the (\code{negative_too} input is a logical
#'   constant of length 1 and not a missing value, is necessary because objects
#'   of type \code{NA} are logical constants of length 1, too.
#'
#' @param x some arbitrary input to be checked.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_neg <- function(x) {
  if (!{
    is.logical(x) && !anyNA(x) && length(x) == 1
  })
    stop("`negative_too` is not a logical vector of length 1.", call. = FALSE)
}
