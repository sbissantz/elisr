#
# TODO: change wfls in utils to scl or whatsoever
# TODO: remove return() -- only in fun applicapble to users
# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
#

# calc_alpha ---------------------------------------------------------------

# calculate Cronbach's Alpha | given: wfls.

  calc_alpha <- function(wfls) {
    cormat <- cor(wfls)
    #lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    m <- length(wfls)
    return((m * rbar) / (1 + rbar * (m - 1)))
  }

# Calculate Corrected Item-Total Correlation ------------------------------

# calculates r_it | given: wfls

  calc_rit <- function(wfls) {
    wf_len <- length(wfls)
    core <- rowSums(wfls[-wf_len])
    addtnl <- wfls[wf_len]
    return(cor(core, addtnl))
  }

# Calculate Average Correlation -------------------------------------------

  calc_rbar <- function(wfls) {
    cormat <- cor(wfls)
    # Calculate: rbar | lower trimat : diag is set FALSE
    return(mean(cormat[lower.tri(cormat)]))
  }

# Extract Items -----------------------------------------------------------

extr_itms <- function(df, itm_nms){
  df[,itm_nms]
}

# Extract Everything But Items --------------------------------------------

extreb_itms <- function(df, itm_nms) {
  df[, -which(names(df) %in% itm_nms)]
}

