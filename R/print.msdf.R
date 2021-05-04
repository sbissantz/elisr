#' @title Print method for a multiple scaled data frame
#'
#' @description The print method for a multiple scaled data frame. It summarizes
#'   the \code{msdf} using values of classical test theory. Note that every line
#'   in the output tags a new stage in the development process of each gradually
#'   emerging scale.
#'
#' @param x a multiple scaled data frame (built with either
#'   \code{disjoint()} or \code{overlap()}).
#'
#' @param digits an integer constant to determine the number of printed digits.
#' See \code{digits} in \code{\link[base]{options}} for details.
#'
#' @param use an optional string to specify how missing values enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to \code{pairwise.complete.obs}.
#'
#' @param ... Additional arguments to the method which will be ignored.
#'
#' @details \code{use} clarifies how to set up a correlation matrix in the
#'   presence of missing values. In a typical scaling process this happens at
#'   least twice. First, when determining the core items (the two items in the
#'   correlation matrix with the highest linear relationship). Second, when an
#'   item is proposed for an emerging scale.
#'
#' @return A list of summary statistics: the marginal corrected item-total
#'   correlation (\code{mrit}), Cronbach's alpha (\code{alpha}), and the average
#'   correlation (\code{rbar}).
#'
#' @references Müller-Schneider, T. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie, 30(4), 305-315.
#'   https://doi.org/10.1515/zfsoz-2001-0404
#'
#'@export
print.msdf <- function(x, digits = 2, use = "pairwise.complete.obs", ...) {
  check_msdf(x)
  explr_once <- function(scl, use) {
   scl_len <- length(scl)
    # Trick: retraces the sequential "emergence" of scldf
    seqls <- lapply(2:scl_len, seq)
    # wfls: a ls of "working frames" imitating a given stage of sequential scale
    # development (i.e.: when a new item was added)
    # therefore: build wfls | that seq of numbers
    wfls <- lapply(seqls, function(seq) scl[seq])
    col_nms <- names(scl)
    col_len <- length(col_nms)
    # "-1": 1st two items are thrown together (1 item: can't be a scale)
    # Assemble: names of the 1st two items (building core) into single row
    # -- provides: an unambiguous output
    var_nms <- paste(col_nms[seq(2)], collapse = ", ")
    if (col_len > 2) {
      var_nms <- c(var_nms, col_nms[seq(3, col_len)])
    }
    mrit <- vapply(wfls, calc_rit, use, FUN.VALUE = double(1))
    alpha <- vapply(wfls, calc_alpha, use, FUN.VALUE = double(1))
    rbar <- vapply(wfls, calc_rbar, use, FUN.VALUE = double(1))
    mat <- cbind(mrit, rbar, alpha)
    row.names(mat) <- var_nms
    mat
  }
  print(lapply(x, explr_once, use), digits = digits)
}
