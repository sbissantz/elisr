#' @title Calculate the corrected item-total correlation
#'
#' @description \code{calc_rit()} calculates the corrected item-total
#'   correlation of the item with the sum score of all the other variables of a
#'   scale or fragment.
#'
#' @details Note, that this procedure uses a part-whole correction. Thus, the
#'   item is excluded from the calculation process.
#'
#' @param scl a scale from a multiple scaled data frame.
#'
#' @param use an optional string indicating how to deal with missing values, See
#'   \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @return A numeric vector of length one. It is used to examine the coherence
#'   between item and overall score.
#'
calc_rit <- function(scl, use) {
  scl_len <- length(scl)
  core <- rowSums(scl[-scl_len])
  addtnl <- scl[scl_len]
  cor(core, addtnl, use = use)
}
