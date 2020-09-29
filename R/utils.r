# TODO: change wfls in utils to scl or whatsoever
# TODO: remove return() -- only in fun applicapble to users
# TODO muscldf -> muscls

# -------------------------------------------------------------------------
# TODO NAME a muscldf?
# Integrate into disjoint(), overlap(), explore()

nme_muscldf <- function(x) {
  x_len <- length(x)
  paste0("scl_", seq(x_len))
}

# Calculate Average Correlation -------------------------------------------

  calc_rbar <- function(scl) {
    cormat <- cor(scl)
    # Calculate: rbar | lower trimat : diag is set FALSE
    mean(cormat[lower.tri(cormat)])
  }

# Calculate Cronbachs Alpha -----------------------------------------------

# calculate Cronbach's Alpha | given: wfls.

  calc_alpha <- function(scl) {
    cormat <- cor(scl)
    #lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    m <- length(scl)
    (m * rbar) / (1 + rbar * (m - 1))
  }

# Calculate Corrected Item-Total Correlation ------------------------------

# calculates r_it | given: wfls

  calc_rit <- function(scl) {
    scl_len <- length(scl)
    core <- rowSums(scl[-scl_len])
    addtnl <- scl[scl_len]
    cor(core, addtnl)
  }

# Extract Items -----------------------------------------------------------

# Subsets a df | given: itm_nms (e.g. from a muscldf)

extr_itms <- function(df, itm_nms) {
  df[, itm_nms]
}

# Extract Everything But Items --------------------------------------------

# Subsets a df | given: everything but itm_nms (e.g. from a muscldf)

extreb_itms <- function(df, itm_nms) {
  df[, -which(names(df) %in% itm_nms)]
}

# Reverse Variables -------------------------------------------------------

# TODO: Called every time! Could I predetermine the value and
# ... and then use this method all the time? TIME PENALTY?
# TODO: Check correctness of sclvals befor reversing

rvrs_var <- function(var, sclvals) {
    # ... -3 -2 -1 0 1 2 3 ...
    if (sclvals[1] < 0) {
      var_rev <- var * -1
    }
    # 0 1 2 3 4 5 6 ...
    if (sclvals[1] == 0 && sclvals[2] > 0) {
      var_rev <- sclvals[2] - var
    }
    # 1 2 3 4 5 6 7
    if (sclvals[1] == 1 && sclvals[2] > 0) {
      var_rev <- (sclvals[2] + 1) - var
    }
  var_rev
}
