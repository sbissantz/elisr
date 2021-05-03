#' @title Rename the components of a multiple scaled data frame
#'
#' @description \code{nme_msdf()} is an internal function. It renames the
#'   components of a msdf
#'
#' @details  The naming convention is \code{scl_n}. \code{scl} stands for
#'   `scale` and \code{n} is the number of the fragment or scale. For example,
#'   the first component is called \code{scl_1}.
#'
#' @param x a multiple scaled data frame.
#'
#' @return A character vector that numbers each element of its input according
#'   to the naming scheme described above.
#'
nme_msdf <- function(x) {
  x_len <- length(x)
  paste0("scl_", seq(x_len))
}
