#' @title Test the appropriateness of an input
#'
#' @description \code{check_mrit()} is an internal function which is called for
#'   its side effects. It guarantees that the input is a double vector of length
#'   one. Moreover, the function secures that the lower bound is unique and
#'   ranges between `0` and `1` (it throws an error if not). In addition, it
#'   warns the user when pre-determining fragments.
#'
#' @param x some arbitrary input to be checked.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_mrit <- function(x) {
  if (!{
    is.double(x) && length(x) == 1
  })
    stop("`mrit_min` is not a double vector of length 1.", call. = FALSE)
  if (!{
    0 <= x && x < 1
  })
    stop("`mrit_min` does not range between `0` and `1`.", call. = FALSE)
  if (x == 0)
    warning("mrit_min = 0: fragment is pre-determined.\n",
            call. = FALSE)
}
