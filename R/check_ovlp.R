#' @title Test the appropriateness of an input
#'
#' @description \code{check_ovlp()} is an internal function which is called for
#'   its side effects. It safeguards that\code{x}, is a character vector of
#'   length `1`. That means, it throws an error if not.
#'
#' @details Within \code{disjoint()} and \code{overlap()}) \code{switch()}
#'   validates the input string itself, throwing an error message when the given
#'   character doesn't match any available option.
#'
#' @param x some arbitrary input to be checked.
#'
#' @return There is no return value. The function is called for its side
#'   effects.
#'
check_ovlp <- function(x) {
  if (!{
    is.character(x) && length(x) == 1
  })
    stop("`overlap_with` is not a character vector of length 1.",
         call. = FALSE)
}
