# calc_alpha ---------------------------------------------------------------

# calculate Cronbach's Alpha | given: wfls.

  calc_alpha <- function(wfls) {
    cormat <- cor(wfls)
    # Calculate: rbar | lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    # Catch: rbar from within calcpha (<<-)
    m <- length(wfls)
    return((m * rbar) / (1 + rbar * (m - 1)))
  }

# Calcrit -----------------------------------------------------------------

# calculates r_it (Corrected Item-Total Correlation) | given: wfls

  calc_rit <- function(wfls) {
    wf_len <- length(wfls)
    core <- rowSums(wfls[-wf_len])
    addtnl <- wfls[wf_len]
    return(cor(core, addtnl))
  }

# calc_rbar ---------------------------------------------------------------

  calc_rbar <- function(wfls) {
    cormat <- cor(wfls)
    # Calculate: rbar | lower trimat : diag is set FALSE
    return(mean(cormat[lower.tri(cormat)]))
  }


