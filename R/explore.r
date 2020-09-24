
# explore() ---------------------------------------------------------------

# takes a scaled data frame (scldf), reconstructs how it was build, and
# returns Cronbach's Alpha, Corrected Item-Total Correlation, and the average
# correlation between items at each stage of the development process.
# TODO: object description 'scldf'
# TODO: Allow to round()
# TODO: Expand explore() to explore all scales in a muscldf at the same time
# -- aim is to forbid only dfs to be read in
# TODO:
#  if (isFALSE(inherits(msdf, "muscldf")))
#    stop("'muscldf' must (literally) be a muscldf.", call. = FALSE)

# TODO in df ther cant be a string (factors might be okay?)

explore <- function(muscldf) {

  stopifnot(inherits(muscldf, "muscldf"))
  op <- options(digits = 2)
  on.exit(options(op))

  explr_once <- function(scldf) {
   scl_len <- length(scldf)
    # Trick: retraces the sequential "emergence" of scldf
    seqls <- lapply(2:scl_len, seq)
    # wfls: a ls of "working frames" (wf) imitating a given stage of the
    # sequential scale development (i.e.: when a new item was added)
    # therefore: build wfls | that seq of numbers
    wfls <- lapply(seqls, function(seq) scldf[seq])
    col_nms <- names(scldf)
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

  return(lapply(muscldf,explr_once))
}


