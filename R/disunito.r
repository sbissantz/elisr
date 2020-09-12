
# Disjoint Scaling Using Negative Items Too -------------------------------

# disunito takes a data.frame and a lower bound in form of a rit_min value as
# well as a 'fullscale value' to allow to reverse the items. It gives back a
# a (list of) multiple scaled data.frame(s). Ellipsis is set for any na.action()
# Hint: One could also set the 'method' argument from cor() but this is not
# tested yet.

disunito <- function(df, rit_min = .3, fullscl_val = NULL, ...) {

# checks ------------------------------------------------------------------

  if (isFALSE(is.data.frame(df)))
    stop("'df' is not a data frame. Please use one.", call. = FALSE)
  if (is.null(names(df)))
    stop("No colnames found. Please specify them.", call. = FALSE)
  if (is.null(fullscl_val))
    stop("No full scale value found. Please specify one.", call. = FALSE)

# object class ------------------------------------------------------------

  # list of disjoint scales & attributes
  lodis <- structure(list(), class = "muscldf", rit_min = rit_min,
                     fullscl_val = fullscl_val, df = match.call()$df,
                     colnames = !is.null(colnames))

# build scales ------------------------------------------------------------

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
          var_rev <- (fullscl_val + 1) - df[fstmaxp]
          lodis[[uni_len]] <- cbind(lodis[[uni_len]], var_rev)
          }
      df <- df[-fstmaxp]
    }
  }
  return(lodis)
}
