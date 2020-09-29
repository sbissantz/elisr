
# Disjoint Scaling Using Negative Items Too -------------------------------

disj_nci <- function(df, rit_min, sclvals, use) {
 scls <- list()
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    # use = use: make sure it's an ARG
    cormat <- cor(df, use = use)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < rit_min) break
    # Take the first(!) maximum
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    scls[[scls_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      scls_len <- length(scls)
      cormat <- cor(rowSums(scls[[scls_len]]), df, use = use)
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < rit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
        }else{
          var_rev <- rvrs_var(var = df[fstmaxp], sclvals)
          scls[[scls_len]] <- cbind(scls[[scls_len]], var_rev)
          }
      df <- df[-fstmaxp]
    }
  }
  scls
}
