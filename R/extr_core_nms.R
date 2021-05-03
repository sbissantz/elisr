#' @title Extract the names of two core items
#'
#' @description \code{extr_core_nms()} is an internal function. It is used to
#'   extract the names of the core items.
#'
#' @param scl a scale within a multiple scaled data frame
#'
#' @return A character vector of length two. It contains the names of the
#'   two items with the highest correlation in a fragment or scale.
#'
extr_core_nms <- function(scl) {
  names(scl)[c(1, 2)]
}
