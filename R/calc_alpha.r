
# calc_alpha ---------------------------------------------------------------

# calculate Cronbach's Alpha | given: wfls.

  calc_alpha <- function(wfls) {
    cormat <- cor(wfls)
    #lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    m <- length(wfls)
    (m * rbar) / (1 + rbar * (m - 1))
  }
