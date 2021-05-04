#' @title Miscellaneous input validation
#'
#' @description A set of test functions to ensure valid input and give helpful
#'   advice if it is not.
#'
#' @return All functions are called for their side effects. If there are no
#'   errors or warnings, no value is returned.
#'
#' @param x some arbitrary input to be checked.
#'
#' @param x_attr a numeric vector of length 2 indicating the start- and endpoint
#'   of a scale.
#'
#' @param mrit_min a numeric constant of length 1 to specify the marginal
#'   corrected item-total correlation.
#'
#' @param use an optional string to specify how missing values enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @details All functions are internal functions.
#'
#' @name checks
#'
NULL

#' @rdname checks
#'
#' @description \code{check_df()} guarantees that \code{x} is an appropriate
#'   data frame for the analysis. That means: It verifies that \code{x} has less
#'   than two variables (a single item can't build a core), \code{x} has column
#'   names (used to pre-build \code{scls} in the overlapping process), if the
#'   column names are unique, and not of type \code{NA}. It throws an error if
#'   any of these requirements are not met. Additionally, it warns the user if
#'   the provided \code{colnames} are not unique or \code{NA}.
#'
check_df <- function(x) {
  if (!is.data.frame(x))
    stop("`df` is not a data.frame.", call. = FALSE)
  if (!length(x) >= 2)
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

#' @rdname checks
#'
#' @description \code{check_sclvals()} tests whether \code{x} is a two element
#'   vector and throws an error if not. Integers are coerced to be of type
#'   \code{double}. Additionally, the function ensures that the first value is
#'   smaller than the second. Remember that checking for a two element vector
#'   implicitly secures that \code{x} is not \code{NULL} (because \code{NULL} is
#'   a logical constant of length `0`).
#'
check_sclvals <- function(x) {
  if (length(x) != 2)
    stop("`sclvals` is not a two element vector.", call. = FALSE)
  if (isFALSE(x[1] < x[2]))
    stop("`sclvals` have to be in the following form `sclvals = c(min,max)`",
         call. = FALSE)
}

#' @rdname checks
#'
#' @description \code{compare_sclvals()} makes sure that the \code{sclvals} set
#'   with \code{overlap()} are equal to those set with \code{disjoint()}. It
#'   throws an error if not.
#'
compare_sclvals <- function(x, x_attr) {
  if (!is.null(x) && !is.null(x_attr)) {
  # Since `:` produces `integer` and `c()` doubles, coerce input.
  if (!identical(x_attr, as.double(x)))
    stop("Specified `sclvals` are not identical to those set before",
         call. = FALSE)
  }
}

#' @rdname checks
#'
#' @description \code{check_mrit()} guarantees that the input is a double vector
#'   of length one. Moreover, the function secures that the lower bound is
#'   unique and ranges between `0` and `1` (it throws an error if not). In
#'   addition, it warns a user pre-determining a fragment.
#'
check_mrit <- function(x) {
  if (!{
    is.double(x) && length(x) == 1
    })
    stop("`mrit_min` is not a double vector of length 1.", call. = FALSE)
  if (!{
    0 <= x && x < 1
    })
     stop("`mrit_min` does not range between `0` and `1`.", call. = FALSE)
  if (x == 0)
     warning("mrit_min = 0: fragment is pre-determined.\n",
             call. = FALSE)
}

#' @rdname checks
#'
#' @description \code{check_ovlp()} safeguards that \code{x} is a character
#'   vector of length `1`. That means, it throws an error if not. Note that
#'   \code{switch()} within \code{disjoint()} and \code{overlap()} takes care
#'   of the input string itself. It throws an error when the given character
#'   doesn't match any available option.
#'
check_ovlp <- function(x) {
  if (!{
    is.character(x) && length(x) == 1
    })
    stop("`overlap_with` is not a character vector of length 1.",
         call. = FALSE)
}

#' @rdname checks
#'
#' @description \code{check_msdf()} guards against inputs that are not of type
#'   `msdf`. It throws an error if not.
#'
check_msdf <- function(x) {
  if (!inherits(x, "msdf"))
    stop("Specified object is not of type `msdf`.", call. = FALSE)
}

#' @rdname checks
#'
#' @description
#' \code{check_neg()} verifies that the input is a logical constant of length 1
#' and not a missing value (this is necessary because objects of type \code{NA}
#' are logical constants of length 1, too).
#'
check_neg <- function(x) {
   if (!{
     is.logical(x) && !anyNA(x) && length(x) == 1
     })
    stop("`negative_too` is not a logical vector of length 1.", call. = FALSE)
}

#' @rdname checks
#'
#' @description \code{check_comp()} examines the correlation matrix,
#'   \code{cor(df)}. It complains (throws an error) if no correlation in
#'   \code{cor(df)} is greater than the specified \code{mrit_min}.
#'
check_comp <- function(x, mrit_min, use) {
  cormat <- cor(x, use = use)
  maxcor <- max(cormat[cormat < 1])
  if (maxcor < mrit_min)
    stop("`mrit_min` is smaller than any correlation found in `df`.",
         call. = FALSE)
}
