
# Disjoint Scaling Process Using Positive Items ---------------------------

# disupi takes a data.frame and a lower bound in form of a rit_min value and
# generates a (list of) multiple scaled data.frame(s). Ellipsis is set for any
# na.action(), Hint: One could also set the 'method' argument from cor() but
# this is not tested yet.

disupi <- function(df, rit_min = .3, ...) {

# helpful feedback --------------------------------------------------------

#  if(isFALSE(requireNamespace("stats", quietly = TRUE)))
#    stop("The 'stats' is required. Please install and load it.", call. = FALSE)
  if (isFALSE(is.data.frame(df)))
    stop("'df' is not a data frame. Please use one.", call. = FALSE)
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
