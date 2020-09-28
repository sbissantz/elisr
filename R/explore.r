
# Explore A Multiple Scaled Data Frame ------------------------------------

explore <- function(muscldf) {
  check_muscldf(muscldf)
  # op <- options(digits = 2)
  # on.exit(options(op))
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
