#' @title Test the appropriateness of an input
#'
#' @description \code{check_msdf()} is an internal function which is called for
#'   its side effects. It throws an error if the input is not of type `msdf`.
#'
#' @param x some arbitrary input to be checked.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_msdf <- function(x) {
  if (!inherits(x, "msdf"))
    stop("Specified object is not of type `msdf`.", call. = FALSE)
}
