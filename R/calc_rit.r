
# Calculate Corrected Item-Total Correlation ------------------------------

# calculates r_it | given: wfls

  calc_rit <- function(wfls) {
    wf_len <- length(wfls)
    core <- rowSums(wfls[-wf_len])
    addtnl <- wfls[wf_len]
    return(cor(core, addtnl))
  }
