
# Disjoint Scaling Process Using Positive Items ---------------------------

# TODO check `...`

disj_pci <- function(df, rit_min = .3, ...) {
  scls <- list()
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    cormat <- cor(df, ...)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < rit_min) break
    # Take the first(!) maximum
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    scls[[scls_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      scls_len <- length(scls)
      cormat <- cor(rowSums(scls[[scls_len]]), df, ...)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < rit_min) break
      fstmaxp <- which(cormat == maxcor)
      scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
      df <- df[-fstmaxp]
      }
    }
  scls
}
