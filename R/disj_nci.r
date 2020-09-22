# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
# Disjoint Scaling Using Negative Items Too -------------------------------

# disunito takes a data.frame and a lower bound in form of a rit_min value as
# well as a 'fullscale value' to allow to reverse the items. It gives back a
# a (list of) multiple scaled data.frame(s). Ellipsis is set for any na.action()
# Hint: One could also set the 'method' argument from cor() but this is not
# tested yet.

disj_nci <- function(df, rit_min = .3, sclvals = NULL, ...) {
 lodis <- list()
  while (ncol(df) >= 2) {
    uni_len <- length(lodis)
    # Specify which values to use "complete","pairwise"
    cormat <- cor(df, ...)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < rit_min) break
    # Take the first(!) maximum
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    lodis[[uni_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      uni_len <- length(lodis)
      cormat <- cor(rowSums(lodis[[uni_len]]), df, ...)
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < rit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        lodis[[uni_len]] <- cbind(lodis[[uni_len]], df[fstmaxp])
        }else{
          var_rev <- rvrs_var(var = df[fstmaxp], sclvals)
          lodis[[uni_len]] <- cbind(lodis[[uni_len]], var_rev)
          }
      df <- df[-fstmaxp]
    }
  }
  return(lodis)
}
