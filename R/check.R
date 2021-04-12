#' @title A set of checks for the appropriateness of inputs
#'
#' @details \code{check_df(x)} guarantees that \code{x} is a data frame. In
#'   addition, it verifies that \code{x} has less than two variables (a single
#'   item can't build a core), \code{x} has column names (used to pre-build
#'   \code{scls} in the overlapping process), if the column names are unique and
#'   not of type \code{NA}.
#'
#'   \code{check_sclvals(x)} checks whether \code{x} is a two element vector
#'   (Note: implicitly assures that it is not \code{NULL}, since \code{NULL} is
#'   a logical constant of length `0`.). Integers are coerced to be of type
#'   \code{double}. Additionally the function makes sure that the first value is
#'   not greater than the second.
#'
#'   \code{compare_sclvals(x)} ensures that the \code{sclvals} set with
#'   \code{overlap()} are equal to those set with \code{disjoint()}.
#'
#'   \code{check_rit(x)} guarantees that the input is a double vector of length
#'   one. Moreover, it secures that the lower bound is unique and ranges between
#'   `0` and `1`.
#'
#'   \code{check_ovlp(x)} safeguards that\code{x}, is a character vector of
#'   length `1`. Within \code{disjoint()} and \code{overlap()}) \code{switch()}
#'   validates the input string itself, throwing an error message when the given
#'   character doesn't match any available option.
#'
#'   \code{check_msdf(x)} guards against inputs that are not of type `msdf`.
#'
#'   \code{check_neg(x)} verifies that the (\code{negative_too} input is a
#'   logical constant of length 1 and not a missing value (this is necessary
#'   because objects of type \code{NA} are logical constants of length 1, too).
#'
#'   \code{check_comp} examines the correlation matrix, \code{cor(df)}. It
#'   complains if no correlation is greater than the specified \code{mrit_min}.
#'
#' @param x some arbitrary input to be checked
#' @param x_attr a numeric vector of length 2 indicating the start- and endpoint
#'   of a scale.
#' @param mrit_min a numeric constant of length 1 to specify the marginal
#'   corrected item-total correlation.
#' @param use an optional string to specify how missing values enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#' @name checks
#'
NULL

#' @rdname checks
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
check_sclvals <- function(x) {
  if (length(x) != 2)
    stop("`sclvals` is not a two element vector.", call. = FALSE)
  if (isFALSE(x[1] < x[2]))
    stop("`sclvals` have to be in the following form `sclvals = c(min,max)`",
         call. = FALSE)
}

#' @rdname checks
compare_sclvals <- function(x, x_attr) {
  if (!is.null(x) && !is.null(x_attr)) {
  # Since `:` produces `integer` and `c()` doubles, coerce input.
  if (!identical(x_attr, as.double(x)))
    stop("Specified `sclvals` are not identical to those set before",
         call. = FALSE)
  }
}

#' @rdname checks
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
check_ovlp <- function(x) {
  if (!{
    is.character(x) && length(x) == 1
    })
    stop("`overlap_with` is not a character vector of length 1.",
         call. = FALSE)
}

#' @rdname checks
check_msdf <- function(x) {
  if (!inherits(x, "msdf"))
    stop("Specified object is not of type `msdf`.", call. = FALSE)
}

#' @rdname checks
check_neg <- function(x) {
   if (!{
     is.logical(x) && !anyNA(x) && length(x) == 1
     })
    stop("`negative_too` is not a logical vector of length 1.", call. = FALSE)
}

#' @rdname checks
check_comp <- function(x, mrit_min, use) {
  cormat <- cor(x, use = use)
  maxcor <- max(cormat[cormat < 1])
  if (maxcor < mrit_min)
    stop("`mrit_min` is smaller than any correlation found in `df`.",
         call. = FALSE)
}
