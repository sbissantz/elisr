# explore() ---------------------------------------------------------------
# takes a scaled data frame (scldf), reconstructs how it was build, and
# returns Cronbach's Alpha, Corrected Item-Total Correlation, and the average
# correlation between items at each stage of the development process.
explore <- function(scldf) {
# calcalpha ---------------------------------------------------------------
# calculate Cronbach's Alpha | given: wfls.
  calcpha <- function(wfls) {
    cormat <- cor(wfls)
    # Calculate: rbar | lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    # Catch: rbar from within calcpha (<<-)
    rbars[length(wfls) - 1] <<- rbar
    m <- length(wfls)
    return((m * rbar) / (1 + rbar * (m - 1)))
  }
# Calcrit -----------------------------------------------------------------
# calculates r_it (Corrected Item-Total Correlation) | given: wfls
  calcrit <- function(wfls) {
    wf_len <- length(wfls)
    grpseed <- rowSums(wfls[-wf_len])
    addtnl <- wfls[wf_len]
    return(cor(grpseed, addtnl))
  }
  scl_len <- length(scldf)
  # Trick: retraces the sequential "emergence" of scldf
  seqls <- lapply(2:scl_len, seq)
  # wfls: a ls of "working frames" (wf) imitating a given stage of the
  # sequential scale development (i.e.: when a new item was added)
  # therefore: build wfls | that seq of numbers
  wfls <- lapply(seqls, function(seq) scldf[seq])
  col_nms <- colnames(scldf)
  col_len <- length(col_nms)
  # "-1": 1st two items are thrown together (1 item: can't be a scale)
  rbars <- vector(length = col_len - 1)
  # Assemble: names of the 1st two items (building grpseed) into single row
  # -- provides: an unambiguous output
  var_nms <- c(paste(col_nms[seq(2)], collapse = ", "),
                 col_nms[seq(3, col_len)])
  alphas <- lapply(wfls, calcpha)
  rits <- lapply(wfls, calcrit)
  return(cbind(var_nms, alphas, rbars, rits))
}
