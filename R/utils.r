# TODO: change wfls in utils to scl or whatsoever
# TODO: remove return() -- only in fun applicapble to users
# TODO muscldf -> muscls

# Calculate Average Correlation -------------------------------------------

  calc_rbar <- function(wfls) {
    cormat <- cor(wfls)
    # Calculate: rbar | lower trimat : diag is set FALSE
    return(mean(cormat[lower.tri(cormat)]))
  }

# Calculate Cronbachs Alpha -----------------------------------------------

# calculate Cronbach's Alpha | given: wfls.

  calc_alpha <- function(wfls) {
    cormat <- cor(wfls)
    #lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    m <- length(wfls)
    (m * rbar) / (1 + rbar * (m - 1))
  }

# Calculate Corrected Item-Total Correlation ------------------------------

# calculates r_it | given: wfls

  calc_rit <- function(wfls) {
    wf_len <- length(wfls)
    core <- rowSums(wfls[-wf_len])
    addtnl <- wfls[wf_len]
    return(cor(core, addtnl))
  }

# Check Data Frame --------------------------------------------------------
# TODO Integrate a possibility to return df only in suits

check_df <- function(df){
  if(!is.data.frame(df))
    stop("`df` is not a data.frame.", call. = FALSE)
  df_len <- length(df)
  if(df_len < 2){
    stop("`df` has less than 2 variables.")
    }
  df_nms <- names(df)
  df_unms <- unique(df_nms)
  if(is.null(names)){
    stop("`df` has no (col)names.", call. = FALSE)
  }else{
    if(!identical(df_nms, df_unms)) {
    warning ("`df` names are not unique.", call. = FALSE)
    }
    if(anyNA(df_nms)){
      warning ("`df` has `NA` names.", call. = FALSE)
    }
  }
}

# Check Scaling Values ----------------------------------------------------
# TODO Name sclvals to scl_ends, scl_ep's, sclrng

# Hint: Since stop() holds execution, no need for if...else... Just make sure
# the order is okay and that the call to the fun is after sclvals are checked

check_sclvals <- function(sclvals){
  if(is.null(sclvals))
    stop("No `sclvals` found. Specify a vector of the form `c(min,max)`",
         call. = FALSE)
  if (!is.vector(sclvals))
    stop("`sclvals` must be a vector of the form `c(min,max)`.",
         call. = FALSE)
  sclvals_len <- length(sclvals)
  if (isFALSE(sclvals_len == 2))
    stop("'sclvals' takes 2 values a `min` and a `max`, e.g., `c(min, max)`.",
           call. = FALSE)
  if(sclvals[1] > sclvals[2])
    stop("`sclvals` min is is greater than its max. Consider `c(min, max)`.",
           call. = FALSE)
}

# Check lower bound -------------------------------------------------------

# Hint: Since stop() holds execution, no need for if...else... Just make sure
# the order is okay and that the call to the fun is after sclvals are checked

check_rit <- function(rit){
  if(is.null(rit))
    stop("No `rit_min` found. Specify a value between `0` and `1`",
         call. = FALSE)
  if (1 < rit || rit < 0)
    stop("`rit_min` must be a positive correlation between `0` and `1`.",
         call. = FALSE)
  rit_len <- length(rit)
  if (rit_len != 1)
    stop("`rit_min` must be a single value between `0` and `1`.",
         call. = FALSE)
}

# Check Overlapping Method ------------------------------------------------

check_ovlp <- function(ovlp) {
  switch(ovlp,




         )


}




# Extract Items -----------------------------------------------------------

# Subsets a df | given: itm_nms (e.g. from a muscldf)

extr_itms <- function(df, itm_nms){
  df[,itm_nms]
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
    if(sclvals[1] < 0) {
      var_rev <- var * -1
    }
    # 0 1 2 3 4 5 6 ...
    if(sclvals[1] == 0 && sclvals[2] > 0){
      var_rev <- sclvals[2] - var
    }
    # 1 2 3 4 5 6 7
    if(sclvals[1] == 1 && sclvals[2] > 0){
      var_rev <- (sclvals[2] + 1) - var
    }
  var_rev
}


