#' @title Calculate the average correlation
#'
#' @description \code{calc_rbar()} is an internal functions. It calculates the
#'   average correlation of a fragment or scale.
#'
#' @param scl a scale of a multiple scaled data frame
#'
#' @param use an optional string indicating how to deal with missing values, See
#'   \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @return A numeric vector of length one that reports the inter-item
#'   correlation.
#'
calc_rbar <- function(scl, use) {
  cormat <- cor(scl, use = use)
  # Calculate: rbar | lower trimat : diag is set FALSE
  mean(cormat[lower.tri(cormat)])
}
