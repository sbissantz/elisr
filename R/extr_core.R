#' @title Extract the first two elements of an object
#'
#' @description \code{extr_core()} is used to extract all pairs of core items
#'   from a fragment.
#'
#' @param scl a scale within a multiple scaled data frame.
#'
#' @param scl a scale from a multiple scaled data frame
#'
#' @return A numeric vector of length two. It contains the two items with the
#'   highest correlation in a fragment or scale.
#'
extr_core <- function(scl) {
  scl[c(1, 2)]
}
