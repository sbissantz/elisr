#' @title Explore A Multiple Scaled Data Frame
#'
#' @description \code{explore} returns a list of summary statistics. It is
#'   tailored for the use with the multiple scaling approach and gives back the
#'   corrected item total correlation (`rit`), Cronbachs Alpha (`alpha`) and
#'   reports the average correlation (`rbar`) for every scale at each step of
#'   the scaling process.
#'
#' @param msdf a multiple scaled data frame (built with either
#'   \code{disjoint} or \code{overlap}).
#'
#' @param use an optional string indicating how to deal with missing values if
#' necessary. See \code{use} in \code{\link[stats]{cor}} for details. The
#' default is set to `pairwise.complete.obs`.
#'
#' @details The \code{use} argument takes control over the treatment of missing
#'   values when correlation matrices are built. In a (usual) scaling process
#'   this happens at least twice: first when determining the core (the two items
#'   of the correlation matrix with the highest linear relationship), and second
#'   when an item is considered to be part of this scale. Since \code{explore}
#'   reconstructs the scale building process to get the summary statistics one
#'   should stay with the options specified before.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.
#'
#' @examples
#' # Exploring a disjoint scaled data frame (`msdf`)
#' msdf <- disjoint(mtcars, rit_min = .4)
#' explore(msdf)
#'
#' # Exploring an overlapping scaled data frame (`msdf_ovlp`)
#' msdf_disj <- disjoint(mtcars, rit_min = .4)
#' msdf_ovlp <- overlap(msdf_disj, rit_min = .7, overlap_with = "full_scale")
#' explore(msdf_ovlp)

#' @export
explore <- function(msdf, digits = 2, use = "pairwise.complete.obs") {
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
    # cbind(var_nms, rit, rbar, alpha)
    mat <- cbind(mrit, rbar, alpha)
    row.names(mat) <- var_nms
    round(mat, digits = digits)
  }
  scls_nms <- nme_msdf(msdf)
  scls <- lapply(msdf, explr_once, use)
  structure(scls, names = scls_nms)
}
