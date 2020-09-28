# Hint: Since stop() holds execution, no need for if...else... Just make sure
# Hint: Checking for != 1 includes NULL since length(NULL) = 0
# the order is okay and that the call to the fun is after sclvals are checked

# Check Data Frame --------------------------------------------------------
# TODO Integrate a possibility to return df only in suits

check_df <- function(x) {
  if(!is.data.frame(x))
    stop("`df` is not a data.frame.", call. = FALSE)
  x_len <- length(x)
  if(isFALSE(x_len > 2))
    stop("`df` has less than 2 variables.")
  x_nms <- names(x)
  if(is.null(x_nms))
    stop("`df` has no (col)names.",
         call. = FALSE)
  x_unms <- unique(x_nms)
  if(!identical(x_nms, x_unms))
    warning ("`df` (col)names are not unique.", call. = FALSE)
  if(anyNA(x_nms))
    warning ("`df` has (col)names of type `NA`.", call. = FALSE)
  }

# Check Scaling Values ----------------------------------------------------
# TODO Name sclvals to scl_ends, scl_ep's, sclrng

check_sclvals <- function(x){
  if(is.null(x)){
    x <- attr(x, "sclvals")
  }
  x_len <- length(x)
  if (isFALSE(x_len == 2))
    stop("`sclvals` is not a two element vector.", call. = FALSE)
  if(isFALSE(x[1] < x[2]))
    stop("`sclvals` has not the form `sclvals = c(min,max)`", call. = FALSE)
}

# Check lower bound -------------------------------------------------------

check_rit <- function(x){
  if (is.null(x)){
    x <- attr(x, "rit_min")
  }
  x_len <- length(x)
  if (isFALSE(is.double(x) && x_len == 1))
    stop("`rit_min` is not a double vector of length 1.", call. = FALSE)
  if (isFALSE(0 < x && x < 1))
     stop("`rit_min`is not in the range of `0` and `1`.", call. = FALSE)
}

# Check Overlapping Method ------------------------------------------------
# Hint: Switch takes care of the specified options `full_scale` or `cores`

check_ovlp <- function(x) {
  x_len <- length(x)
  if (isFALSE(is.character(x) && x_len == 1))
    stop("`overlap_with` is not a character vector of length 1.", call. = FALSE)
}

# Check Multiple Scaled Data Frame ----------------------------------------
# Hint: No checks for muscldf necessary. A df is only scaled when it has names
# see: check_df(). Since (1) disjoint, overlap doesn't manipulate `colnames` and
# the user cant specify an alternative `df`, names must  be valid.

check_muscldf <- function(x) {
  if (isFALSE(inherits(x, "muscldf")))
    stop("Specified object is not of type `muscldf`.", call. = FALSE)
}

# check_neg ---------------------------------------------------------------

check_neg <- function(x) {
  x_len <- length(x)
  if (isFALSE(is.logical(x) && !is.na(x) && x_len == 1))
    stop("`negative_too` is not a logical vector of length 1.", call. = FALSE)
}
