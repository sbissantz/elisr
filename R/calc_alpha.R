#' @title Calculate Cronbach's alpha
#'
#' @description \code{calc_rbar()} is an internal function. It calculates the
#'   average correlation between items for a gradually emerging scale.
#'
#' @param scl a scale within a multiple scaled data frame.
#'
#' @param use an optional string indicating how to deal with missing values, See
#'   \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @return A numeric vector of length one. It is used to assess the internal
#'   consistency of a scale.
#'
calc_alpha <- function(scl, use) {
  cormat <- cor(scl, use = use)
  #lower trimat : diag is set FALSE
  rbar <- mean(cormat[lower.tri(cormat)])
  m <- length(scl)
  (m * rbar) / (1 + rbar * (m - 1))
}
