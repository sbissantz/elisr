#' @title Explore A Multiple Scaled Data Frame
#'
#' @description \code{explore} returns a list of summary statistics. It is
#'   tailored for the use in the multiple scaling approach and gives back the
#'   corrected item total correlation (`rit`), Cronbachs Alpha (`alpha`) and
#'   reports the average correlation (`rbar`) for every scale at each step of
#'   the scaling process.
#'
#' @param muscldf a multiple scaled data frame (built with either
#'   \code{disjoint} or \code{overlap}).
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.
#'
#' @examples
#' # Exploring a disjoint scaled data frame (`muscldf`)
#' muscldf <- disjoint(mtcars, rit_min = .4)
#' explore(muscldf)
#'
#' # Exploring an overlapping scaled data frame (`msdf_ovlp`)
#' msdf_disj <- disjoint(mtcars, rit_min = .4)
#' msdf_ovlp <- overlap(msdf_disj, rit_min = .7, overlap_with = "full_scale")
#' explore(msdf_ovlp)

explore <- function(muscldf) {
  check_muscldf(muscldf)
  explr_once <- function(scl) {
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
    var_nms <- c(paste(col_nms[seq(2)], collapse = ", "),
                 col_nms[seq(3, col_len)])
    rit <- lapply(wfls, calc_rit)
    alpha <- lapply(wfls, calc_alpha)
    rbar <- lapply(wfls, calc_rbar)
    cbind(var_nms, rit, rbar, alpha)
  }
  scls_nms <- nme_muscldf(muscldf)
  scls <- lapply(muscldf, explr_once)
  structure(scls, names = scls_nms)
}
