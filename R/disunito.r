
# Disjoint Scaling Using Negative Items Too -------------------------------

# disunito takes a data.frame and a lower bound in form of a rit_min value as
# well as a 'fullscale value' to allow to reverse the items. It gives back a
# a (list of) multiple scaled data.frame(s). Ellipsis is set for any na.action()
# Hint: One could also set the 'method' argument from cor() but this is not
# tested yet.

disunito <- function(df, rit_min = .3, sclvals = NULL, ...) {

# checks ------------------------------------------------------------------

  if (isFALSE(is.data.frame(df)))
    stop("'df' is not a data frame. Please use one.", call. = FALSE)
  # TODO: Keep that as a colnames check in the function not in the constructor!
  if (is.null(names(df)))
    stop("No colnames found. Please specify them.", call. = FALSE)
  if (is.null(sclvals))
    stop("No 'sclvals' found. Please specify one.", call. = FALSE)
  # if (sclvals <= 0)
  #  stop("disunito() can only deal with 'values > 0'. Please recode the scale",
  #       call. = FALSE)

# object class ------------------------------------------------------------

 # # list of disjoint scales & attributes
 # lodis <- structure(list(), class = "muscldf", rit_min = rit_min,
 #                    sclvals = sclvals, df = match.call()$df,
 #                    colnames = !is.null(colnames))

 lodis <- list()

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
          var_rev <- rvrs_var(var = df[fstmaxp], sclvals)
          lodis[[uni_len]] <- cbind(lodis[[uni_len]], var_rev)
          }
      df <- df[-fstmaxp]
    }
  }
  return(lodis)
}
