#' @title Test for a multiple scaled data frame object
#'
#' @description \code{is.msdf()} tests if the given object is a multiple scaled
#'   data frame.
#'
#' @param x an arbitrary object.
#'
#' @details \code{is.msdf} returns \code{TRUE} if the given object is of type
#'   \code{msdf} and FALSE otherwise.

#'@export
is.msdf <- function(x) inherits(x, "msdf")
