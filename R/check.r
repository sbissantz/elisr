#' @title A Set of Checks
#'
#' @description A set of helper functions for checking the appropriateness of
#'   inputs. It is used all around
#'
#' @param check_df(x), checks whether \code{x} is a data frame, if it has less
#'   than two variables (a single item can't set up a core), whether \code{x}
#'   has column names (used to pre-build \code{scls} in the overlapping
#'   process), if column names are unique and not \code{NA}.
#'
#' @param check_sclvals(x) checks whether \code{x} is a two element vector
#'   (implicitly assures that it is not \code{NULL}, since \code{NULL} is a
#'   logical constant of length `0`.). Integers are coerced to be of type
#'   \code{double}. Additionally the function makes sure that the first value is
#'   not greater than the second.
#'
#' @param compare_sclvals(x) is used when the user sets \code{sclvals} in an
#'   overlapping process, but there are already \code{sclvals} given from the
#'   disjoint scaling process done before. The function assures that the
#'   attribute is identical to the input (it doesn't make sense to change
#'   \code{sclvals} in the process of analysis.)
#'
#' @param check_rit(x) makes sure that the input is a double vector of length
#'   one (a user can only specify one lower bound in a scaling process) and in
#'   the range between `0` and `1`.
#'
#' @param check_ovlp(x) assures that the method to `overlap_with`, i.e.
#'   \code{x}, is a character vector of length `1`. Any further checks are
#'   required since \code{switch} (used in the main functions \code{disjoint}
#'   and \code{overlap}) validates the input strings itself, throwing an error
#'   message when the given character doesn't match the available options
#'
#' @param check_muscldf(x) is used in \code{explore} and \code{overlap} to make
#'   sure the specified input is of type `muscldf`. Hint: No further checks for
#'   muscldf necessary. A data frame is only scaled when it has names see:
#'   \code{check_df}. Since (1) \code{disjoint} and \code{overlap} do not
#'   manipulate the `colnames` attribute and the user cant specify an
#'   alternative `df`, names must  be valid.
#'
#' @param check_neg(x) checks the negative_too input is a logical constant of
#'   length 1. Since any \code{NA} value is a logical constant of length `0` one
#'   must additionally assure that the input is not a \code{NA}.

check_df <- function(x) {
  if (!is.data.frame(x))
    stop("`df` is not a data.frame.", call. = FALSE)
  x_len <- length(x)
  if (isFALSE(x_len > 2))
    stop("`df` has less than 2 variables.")
  x_nms <- names(x)
  if (is.null(x_nms))
    stop("`df` has no (col)names.",
         call. = FALSE)
  x_unms <- unique(x_nms)
  if (!identical(x_nms, x_unms))
    warning("`df` (col)names are not unique.", call. = FALSE)
  if (anyNA(x_nms))
    warning("`df` has (col)names of type `NA`.", call. = FALSE)
  }

# Check Scaling Values ----------------------------------------------------
# TODO Name sclvals to scl_ends, scl_ep's, sclrng

check_sclvals <- function(x) {
  x_len <- length(x)
  if (isFALSE(x_len == 2))
    stop("`sclvals` is not a two element vector.", call. = FALSE)
  if (isFALSE(x[1] < x[2]))
    stop("`sclvals` has not the shape `sclvals = c(min,max)`", call. = FALSE)
}

# Compare Sclaing Value with Attribute ------------------------------------

compare_sclvals <- function(x, x_attr) {
  if (!is.null(x) && !is.null(x_attr)) {
  # Since `:` produces `integer` and `c()` doubles, coerce input.
  x_dbl <- as.double(x)
  ident_sclvals <- identical(x_attr, x_dbl)
  if (isFALSE(ident_sclvals))
    stop("Specified `sclvals` are not identical to those set before",
         call. = FALSE)
  }
}

# Check lower bound -------------------------------------------------------

check_rit <- function(x) {
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
