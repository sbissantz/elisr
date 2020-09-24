
# Calculate Average Correlation -------------------------------------------

  calc_rbar <- function(wfls) {
    cormat <- cor(wfls)
    # Calculate: rbar | lower trimat : diag is set FALSE
    return(mean(cormat[lower.tri(cormat)]))
  }
