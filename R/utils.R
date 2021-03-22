#' @title A Set of Utility Functions
#'
#' @details \code{nme_muscldf} names a muscldf. It is used for convenience and
#'   to replace the common list element indicator, e.g.,`[[1]]`, by assigning a
#'   name to the scale, e.g., `scl_1`.
#'
#'   \code{calc_rbar} calculates the average correlation between the items at a
#'   certain point of the scaling process.
#'
#'   \code{calc_alpha} calculates Cronbachs Alpha of a scale (i.e., the internal
#'   consistency of the items) at a certain point of the scaling process.
#'
#'   \code{calc_mrit} calculates the Corrected Item-Total Correlation of an item
#'   with the sum score of all the other variables of the scale -- at a certain
#'   point of the scaling process. It uses a part-whole correction, i.e., the
#'   item itself is excluded in the calculation process.
#'
#'   \code{extr_itms} subsets a data frame given a set of item names from a
#'   scale built in the disjoint scaling process (i.e., from a muscldf). It is
#'   used to construct one of the `working frames` in the overlapping process.
#'   Being more precise, \code{extr_itms} collects (instructed by the
#'   `overlap_with` argument) if only the cores or the full_scale should be used
#'   as a base to overlap.
#'   DELETE IF TESTED!
#'
#'   \code{extr_core} extracts (multiple) pairs of core items from a given data
#'   frame (`scl`), which is often part of a list of data frames nested within a
#'   `msdf` object. `overlap()` draws on this helper function when setting the
#'   `overlap_with=core` option. Thus, it forms the base for the upcoming
#'   overlap procedure.
#'
#'   \code{extr_core_nms} extracts the name of the core item from a given data
#'   frame (`scl`), which is often part of a list of data frames nested within a
#'   `msdf` object. `overlap()` draws on this helper function when setting the
#'   `overlap_with=core` option. It litterally sets up the counterpart for the
#'   upcoming overlap procedure.
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

# extr_itms <- function(df, itm_nms) {
#  df[itm_nms]
#  # df[, itm_nms, drop = FALSE]
# }
# DELETE IF TESTED!

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
  # df[, -which(names(df) %in% itm_nms), drop = FALSE]
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
