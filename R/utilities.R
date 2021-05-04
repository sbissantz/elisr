#' @title Miscellaneous utility functions
#'
#' @description A set of utility functions to calculate characteristic values of
#'   classical test theory, reverse a variable, and extract the relevant bits
#'   from various objects.
#'
#' @details All functions are internal functions.
#'
#' @param scl a scale within a multiple scaled data frame.
#'
#' @param use an optional string indicating how to deal with missing values, See
#'   \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @param var a variable or item (often a column from a data frame).
#'
#' @param sclvals the start- and end point of a scale (specify:
#'   \code{c(sp,ep)}).
#'
#' @param msg a reverse message sent from either \code{disjoint()} or
#'   \code{overlap()}.
#'
#' @param applicant the function which wants to leave messages \code{disjoint()}
#'   or \code{overlap()}.
#'
#' @param df a data frame object.
#'
#' @param itm_nms the names of an item from a scale.
#'
#' @param x a multiple scaled data frame.
#'
#' @name utilities
#'
NULL

#' @rdname utilities
#'
#' @description \code{calc_rit()} calculates the corrected item-total
#'   correlation of a scale or fragment using a part-whole correction. Thus, the
#'   item itself is excluded in the calculation process.
#' @returns \code{calc_rit()} returns a numeric vector of length one. It is used
#'   to examine the coherence between item and overall score.
#'
  calc_rit <- function(scl, use) {
    scl_len <- length(scl)
    core <- rowSums(scl[-scl_len])
    addtnl <- scl[scl_len]
    cor(core, addtnl, use = use)
  }

#' @rdname utilities
#'
#' @description \code{calc_rbar()} calculates the average correlation of a
#'   fragment or scale.
#' @returns \code{calc_rbar()} returns a numeric vector of length one that
#'   reports the inter-item correlation.
#'
  calc_rbar <- function(scl, use) {
    cormat <- cor(scl, use = use)
    # Calculate: rbar | lower trimat : diag is set FALSE
    mean(cormat[lower.tri(cormat)])
  }

#' @rdname utilities
#'
#' @description \code{calc_alpha()} calculates the internal consistency of a
#' scale or fragment using Cronbach's alpha.
#' @return \code{calc_alpha()} returns a numeric vector of length one which is
#'   used to assess the internal consistency of a scale.
#'
  calc_alpha <- function(scl, use) {
    cormat <- cor(scl, use = use)
    #lower trimat : diag is set FALSE
    rbar <- mean(cormat[lower.tri(cormat)])
    m <- length(scl)
    (m * rbar) / (1 + rbar * (m - 1))
  }

#' @rdname utilities
#'
#' @description
#'   \code{rvrs_var()} reverses an item using the specified scaling values. It
#'   handles the following types of scales:
#'   \itemize{
#'   \item  ...-3 -2 -1 0 1 2 3..., e.g., \code{sclvals = c(-3, 3)}
#'   \item  0 1 2 3 4 5 6..., e.g.,  \code{sclvals = c(0, 7)}
#'   \item  1 2 3 4 5 6 7..., e.g., \code{sclcals = c(1, 7)}
#'   }
#' @return \code{rvrs_var()} returns the reversed numeric vector using the above
#'   reversing scheme.
#'
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

#' @rdname utilities
#'
#' @description \code{rvrs_note()} gets the full report of reversed variables
#'   and reports a unique list of them.
#' @returns \code{rvrs_note()} is called for its side effects. It leaves a
#'   message when an item is reversed.
#'
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

#' @rdname utilities
#'
#' @description
#'   \code{extr_core()} is used to extract all pairs of core items from a
#'   fragment.
#' @returns \code{extr_core()} returns a numeric vector of length two, which
#'   contains the two items with the highest correlation in a fragment or scale.
#'
  extr_core <- function(scl) {
    scl[c(1, 2)]
  }

#' @rdname utilities
#'
#' @description \code{extr_core_nms()} is used to extract the names of all pairs
#'   of core items from a given fragment.
#' @returns \code{extr_core_nms()} returns character vector of length two, which
#'   contains the names of the two items with the highest correlation in a
#'   fragment or scale.
#'
  extr_core_nms <- function(scl) {
    names(scl)[c(1, 2)]
  }

#' @rdname utilities
#'
#' @description \code{extreb_itms()} builds the counterpart of a fragment from
#'   the given item names. Therefore, the counterpart includes all variables
#'   that are not part of a fragment but which are mentioned in the specified
#'   data set.
#' @returns \code{extrev_itms()} returns vector of length m minus two, where 'm'
#'   specifies the number of variables in a given data set. It contains the
#'   items of a scale's counterpart.
#'
  extreb_itms <- function(df, itm_nms) {
    df[-which(names(df) %in% itm_nms)]
  }

#' @rdname utilities
#'
#' @description \code{nme_msdf()} renames the components of a multiple scaled
#'   data frame. The naming scheme is \code{scl_n}. \code{scl} stands for
#'   `scale` and \code{n} specifies the number of fragments or scales. For
#'   example, the first component is called \code{scl_1}.
#' @returns \code{nme_msdf()} returns a character vector that numbers each
#'   element of its input according to the above naming scheme.
#'
nme_msdf <- function(x) {
  x_len <- length(x)
  paste0("scl_", seq(x_len))
}
