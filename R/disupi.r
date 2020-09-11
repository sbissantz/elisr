# Disjoint Scaling Process Using Positive Items ---------------------------

disupi <- function(df, rit_min = .3, ...) {

# helpful feedback --------------------------------------------------------

  if(isFALSE(is.data.frame(df)))
    stop("'df' is not a data frame. Please create one.", call. = FALSE)
  if (is.null(names(df)))
    stop("No colnames found. Please specify them.", call. = FALSE)

# lodis & attrbitues ------------------------------------------------------
# List of disjoint scales with attributes

  lodis <- structure(list(), class = "muscldf", rit_min = rit_min,
                     df = match.call()$df, colnames = !is.null(colnames))

# scaling procedure -------------------------------------------------------

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
