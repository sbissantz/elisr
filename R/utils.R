#' @title A set of utility functions
#'
#' @details \code{nme_msdf()} designates the components of a msdf (e.g.,
#'   `scl_1`).
#'
#'   \code{calc_rbar()} calculates the average correlation between items from a
#'   gradually emerging scale.
#'
#'   \code{calc_alpha()} calculates Cronbach's alpha of a given set of items from
#'   the gradually emerging scale.
#'
#'   \code{calc_mrit()} calculates the corrected item-total correlation of an item
#'   with the sum score of all the other variables of the gradually emerging
#'   scale (aka. marginal corrected item-total correlation). Note, that this
#'   procedure uses a part-whole correction. Thus, the item itself is excluded
#'   in the calculation process.
#'
#'   \code{extreb_itms()} build up the counterpart of a fragment from the given
#'   item names. Therefore, the counterpart includes all variables that are not
#'   part of a fragment but which are mentioned in the specified data set.
#'
#'   \code{extr_core()} extracts all pairs of core items from a fragment.
#'
#'   \code{extr_core_nms()} extracts the names of every core items in a given
#'   fragment.
#'
#'   \code{rvrs_var()} reverses an item using the specified scaling values. It
#'   handles the following types of scales:
#'   \itemize{
#'   \item  ...-3 -2 -1 0 1 2 3..., e.g., \code{sclvals = c(-3, 3)}
#'   \item  0 1 2 3 4 5 6..., e.g.,  \code{sclvals = c(0, 7)}
#'   \item  1 2 3 4 5 6 7..., e.g., \code{sclcals = c(1, 7)}
#'   }
#'
#'   \code{rvrs_note()} gets the full report of reversed variables and reports a
#'   unique list of them.
#'
#' @param x a multiple scaled data frame
#' @param scl a scale within a multiple scaled data frame
#' @param use an optional string indicating how to deal with missing values, See
#'   \code{use} in \code{\link[stats]{cor}} for details.
#' @param df a data frame object
#' @param itm_nms the names of an item from a scale
#' @param var a variable or item (often a column from a data frame)
#' @param sclvals the start and end point of a scale (specify: \code{c(sp,ep)})
#' @param applicant the function which wants to leave messages \code{disjoint}
#'   or \code{overlap})
#' @param msg a reverse message sent from either \code{disjoint} or
#'   \code{overlap}
#' @name utils
NULL

#' @rdname utils
nme_msdf <- function(x) {
  x_len <- length(x)
  paste0("scl_", seq(x_len))
}

#' @rdname utils
  calc_rbar <- function(scl, use) {
    cormat <- cor(scl, use = use)
    # Calculate: rbar | lower trimat : diag is set FALSE
    mean(cormat[lower.tri(cormat)])
  }

#' @rdname utils
  calc_alpha <- function(scl, use) {
    cormat <- cor(scl, use = use)
    #lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    m <- length(scl)
    (m * rbar) / (1 + rbar * (m - 1))
  }

#' @rdname utils
  calc_mrit <- function(scl, use) {
    scl_len <- length(scl)
    core <- rowSums(scl[-scl_len])
    addtnl <- scl[scl_len]
    cor(core, addtnl, use = use)
  }

  #' @rdname utils
  extr_core <- function(scl) {
    scl[c(1, 2)]
  }

  #' @rdname utils
  extr_core_nms <- function(scl) {
    names(scl)[c(1, 2)]
  }

  #' @rdname utils
  extreb_itms <- function(df, itm_nms) {
    df[-which(names(df) %in% itm_nms)]
  }

  #' @rdname utils
  rvrs_var <- function(var, sclvals) {
    # ... -3 -2 -1 0 1 2 3 ...
    if (sclvals[1] < 0) {
      return(var * -1)
    }
    # 0 1 2 3 4 5 6 ...
    if (sclvals[1] == 0 && sclvals[2] > 0) {
      return(sclvals[2] - var)
    }
    # 1 2 3 4 5 6 7
    if (sclvals[1] == 1 && sclvals[2] > 0) {
      return((sclvals[2] + 1) - var)
    }
  }

  #' @rdname utils
  rvrs_note <- function(msg, applicant) {
    msg_len <- length(msg)
    if (msg_len == 0) {
      message(paste0("\n", applicant, "() didn't reverse an item.\n"))
    } else {
      message(
        paste0("\n", applicant, "() reversed the following item(s):\n"),
        paste0("- ", unique(msg), "\n"))
    }
  }
