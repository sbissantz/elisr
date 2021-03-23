#' @title A Set of Utility Functions
#'
#' @details \code{nme_muscldf} designates the components of a muscldf (e.g.,
#'   `scl_1`).
#'
#'   \code{calc_rbar} calculates the average correlation between items from a
#'   gradually emerging scale.
#'
#'   \code{calc_alpha} calculates Cronbachs alpha of a given set of items from
#'   the gradually emerging scale.
#'
#'   \code{calc_mrit} calculates the corrected item-total correlation of an item
#'   with the sum score of all the other variables of the gradually emerging
#'   scale. That's why we call it marginal rit. Note that this procedure uses a
#'   part-whole correction. Thus the item itself is excluded in the calculation
#'   process.
#'
#'   \code{extreb_itms} build up the counterpart of a fragment using its items
#'   names. Therefore the counterpart includes all variables that are not
#'   deliverd with the fragment but which are mentioned in the specified data
#'   set.
#'
#'   \code{extr_core} extracts pairs of core items from a data frame. Those are
#'   part of a list of data frames nested within a `msdf` object. `overlap()`
#'   draws on this helper function when setting the `overlap_with=core` option.
#'   Hence, it forms for the actual overlap.
#'
#'   \code{extr_core_nms} extracts the names of the core items in a given
#'   fragment (e.g. `scl_1`).
#'
#'   \code{rvrs_var} reverses the item given the specified scaling values. It
#'   can handle the following types of scales:
#'   \itemize{
#'   \item  ...-3 -2 -1 0 1 2 3..., e.g., \code{sclvals = c(-3, 3)}
#'   \item  0 1 2 3 4 5 6..., e.g.,  \code{sclvals = c(0, 7)}
#'   \item  1 2 3 4 5 6 7..., e.g., \code{sclcals = c(1, 7)}
#'   }
#'
#'   \code{rvrs_note} gets a report of variables `rvrs_var` recoded. Note
#'   however, that `rvrs_note` presentes this results only in a reduced form. To
#'   not overload the output it reports only a unique list of its notes.
#'
#' @param x a muscldf
#' @param scl a scale from a muscldf
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details.
#' @param df a data frame object,
#' @param itm_nms the names of an item from a scale.
#' @param var a variable or item (often a column from a data frame)
#' @param sclvals the start and end point of a scale (consider \code{c(sp,ep)})
#' @name utils
NULL

#' @rdname utils
nme_muscldf <- function(x) {
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
  scl[c(1,2)]
}

#' @rdname utils
extr_core_nms <- function(scl) {
  names(scl)[c(1,2)]
}

#' @rdname utils
extreb_itms <- function(df, itm_nms) {
  df[-which(names(df) %in% itm_nms)]
}

#' @rdname utils
rvrs_var <- function(var, sclvals) {
    # ... -3 -2 -1 0 1 2 3 ...
    if (sclvals[1] < 0) {
      var_rev <- var * -1
    }
    # 0 1 2 3 4 5 6 ...
    if (sclvals[1] == 0 && sclvals[2] > 0) {
      var_rev <- sclvals[2] - var
    }
    # 1 2 3 4 5 6 7
    if (sclvals[1] == 1 && sclvals[2] > 0) {
      var_rev <- (sclvals[2] + 1) - var
    }
  var_rev
}

#' @rdname utils
rvrs_note <- function(msg, applicant) {
  msg_len <- length(msg)
  if (msg_len == 0) {
    message(paste0("\n", applicant, "() didn't reverse an item.\n"))
  } else {
    message(
      paste0("\n", applicant, "() reversed the following items internally:\n"),
      paste0("– ", unique(msg), "\n"))
  }
}
