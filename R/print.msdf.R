#' @title Print method for multiple scaled data frame (msdf) objects
#'
#' @description The print method for msdf objects returns a list of summary
#'   statistics. It is tailored for the use with the multiple scaling approach
#'   and returns the marginal corrected item total correlation (\code{mrit}),
#'   Cronbachs Alpha (`alpha`) and reports the average correlation (`rbar`) for
#'   every scale at each step of the scaling process.
#'
#' @param msdf a multiple scaled data frame (built with either
#'   \code{disjoint} or \code{overlap}).
#'
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details. The
#'   default is set to `pairwise.complete.obs`.
#'
#' @details The \code{use} argument takes control over the treatment of missing
#'   values when correlation matrices are built. In a (usual) scaling process
#'   this happens at least twice: first when determining the core (the two items
#'   of the correlation matrix with the highest linear relationship), and second
#'   when an item is considered to be part of a given scale. Since the print is
#'   programmed to reconstructs the scale building process (done with
#'   \code{disjoint} or \code{overlap}) to get its summary statistics, one
#'   should stay with the specified options.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.
#'
#' @examples
#' # Exploring a disjoint scaled data frame (`msdf`)
#' msdf <- disjoint(mtcars, mrit_min = .4)
#' explore(msdf)
#'
#' # Exploring an overlapping scaled data frame (`msdf_ovlp`)
#' msdf_disj <- disjoint(mtcars, mrit_min = .4)
#' msdf_ovlp <- overlap(msdf_disj, mrit_min = .7, overlap_with = "full_scale")
#' explore(msdf_ovlp)

#'@export
print.msdf <- function(msdf, digits = 2, use = "pairwise.complete.obs") {
  check_msdf(msdf)
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
    mrit <- vapply(wfls, calc_mrit, use, FUN.VALUE = double(1))
    alpha <- vapply(wfls, calc_alpha, use, FUN.VALUE = double(1))
    rbar <- vapply(wfls, calc_rbar, use, FUN.VALUE = double(1))
    mat <- cbind(mrit, rbar, alpha)
    row.names(mat) <- var_nms
    mat
  }
  print(lapply(msdf, explr_once, use), digits = digits)
}
