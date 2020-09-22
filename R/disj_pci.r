# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"

# Disjoint Scaling Process Using Positive Items ---------------------------

# disupi takes a data.frame and a lower bound in form of a rit_min value and
# generates a (list of) multiple scaled data.frame(s). Ellipsis is set for any
# na.action(), Hint: One could also set the 'method' argument from cor() but
# this is not tested yet.

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
      cormat <- cor(rowSums(scls[[scls_len]]), df)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < rit_min) break
      fstmaxp <- which(cormat == maxcor)
      scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
      df <- df[-fstmaxp]
      }
    }
  return(scls)
}
