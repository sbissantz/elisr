#' @title Test the appropriateness of the relation between two inputs
#'
#' @description \code{compare_sclvals()} is an internal function which is called
#'   for its side effects. It ensures that the \code{sclvals} set with
#'   \code{overlap()} are equal to those set with \code{disjoint()} -- and
#'   throws an error if not.
#'
#' @param x some arbitrary input to be checked.
#'
#' @param x_attr a numeric vector of length 2 indicating the start- and
#'   endpoint.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
compare_sclvals <- function(x, x_attr) {
  if (!is.null(x) && !is.null(x_attr)) {
    # Since `:` produces `integer` and `c()` doubles, coerce input.
    if (!identical(x_attr, as.double(x)))
      stop("Specified `sclvals` are not identical to those set before",
           call. = FALSE)
  }
}
