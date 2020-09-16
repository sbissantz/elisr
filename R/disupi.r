# TODO: FUN has Name is from utils
# Disjoint Scaling Process Using Positive Items ---------------------------

# disupi takes a data.frame and a lower bound in form of a rit_min value and
# generates a (list of) multiple scaled data.frame(s). Ellipsis is set for any
# na.action(), Hint: One could also set the 'method' argument from cor() but
# this is not tested yet.

# TODO: Names
# shouldnt be NA
# should be non-Null
# should be unique

disupi <- function(df, rit_min = .3, ...) {

# helpful feedback --------------------------------------------------------

  stopifnot(is.data.frame(df))
  df_nms <- names(df)
  if (anyNA(df_nms))
    stop("`NA` is not a valid name for a column.", call. = FALSE)

  lodis <- list()

  while (ncol(df) >= 2) {
    (uni_len <- length(lodis))
    (cormat <- cor(df, ...))
    (maxcor <- max(cormat[cormat < 1]))
    if (maxcor < rit_min) break
    # Take the first(!) maximum
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    lodis[[uni_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      uni_len <- length(lodis)
      cormat <- cor(rowSums(lodis[[uni_len]]), df)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < rit_min) break
      fstmaxp <- which(cormat == maxcor)
      lodis[[uni_len]] <- cbind(lodis[[uni_len]], df[fstmaxp])
      df <- df[-fstmaxp]
      }
    }
  return(lodis)
}
