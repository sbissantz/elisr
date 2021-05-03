#' @title Test the appropriateness of an input
#'
#' @description \code{check_sclvals()} is an internal function which is called
#'   for its side effects. It checks whether \code{x} is a two element vector
#'   and throws an error if not. Integers are coerced to be of type
#'   \code{double}. Additionally, the function makes sure that the first value
#'   is not greater than the second.
#'
#' @details Note that, when it checks whether \code{x} is a two element vector
#'   it implicitly assures that it is not \code{NULL}, since \code{NULL} is
#'   a logical constant of length `0`.
#'
#' @param x some arbitrary input to be checked.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_sclvals <- function(x) {
  if (length(x) != 2)
    stop("`sclvals` is not a two element vector.", call. = FALSE)
  if (isFALSE(x[1] < x[2]))
    stop("`sclvals` have to be in the following form `sclvals = c(min,max)`",
         call. = FALSE)
}
