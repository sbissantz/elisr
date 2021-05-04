#' @title elisr's quadriga
#'
#' @description The four workhorses inside \code{elisr}'s user functions
#'   \code{disjoint()} and \code{overlap()}.
#'
#' @details All functions are internal functions.
#'
#' The \code{use} argument specifies how to set up a correlation matrix in the
#' presence of missing values. In a typical scaling process this happens at
#' least twice. First, when determining the core items (the two items in the
#' correlation matrix with the highest linear relationship). Second, when an
#' item is proposed for an emerging scale.
#'
#' Note that all functions use \code{\link[stats]{cor}}'s default method
#' \code{pearson}.
#'
#' @param df a data frame object.
#'
#' @param mrit_min a numerical constant to specify the marginal corrected item
#'   total correlation. The value must be in the range of 0-1.
#'
#' @param use an optional string to specify how missing values will enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @param sclvals a numerical vector of length 2 indicating the start- and
#'   endpoint of a scale.
#'
#' @param use an optional string to specify how missing values will enter the
#'   analysis. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @param msdf a multiple scaled data frame (built with \code{disjoint()}).
#'
#' @param overlap_with a string telling \code{overlap()} the set of items for
#'   the extension. To build up on all variables of a fragment use
#'   \code{fragment}, for the core-only option type \code{core}. The default is
#'   set to "fragment".
#'
#' @returns \code{disj_pci()} and \code{disj_nci()} both return a list of data
#'   frames which result from applying the above-mentioned algorithm.
#'
#' @returns \code{ovlp_pci()} and \code{ovlp_nci()} often return an
#'   \emph{extended} a list of data frames.
#'
#' @references Müller-Schneider, T. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie, 30(4), 305-315.
#'   https://doi.org/10.1515/zfsoz-2001-0404
#'
#' @name workhorses
#'
NULL

#' @rdname workhorses
#'
#' @description \code{disj_pci()} is a loop which runs through the following
#'   steps: (1) Set up a (first) scale. (2) Find the two items with the highest
#'   positive correlation in the data set. (3) If the absolute value of this
#'   correlation is greater than the pre-specified lower bound
#'   (\code{mrit_min}), add up the two items to build the core of the emerging
#'   scale. (4) As long as the value of the correlation between the sum-score
#'   and a remaining item in the data frame is greater than \code{mrit_min},
#'   flavor the scale with the appropriate item. (5) If there are at least two
#'   leftovers in the data frame that meet the inclusion criterion, start over
#'   again.
#'
#' @importFrom stats cor
disj_pci <- function(df, mrit_min, use) {
  scls <- list()
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    cormat <- cor(df, use = use)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < mrit_min) break
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    scls[[scls_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      scls_len <- length(scls)
      cormat <- cor(rowSums(scls[[scls_len]]), df, use = use)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < mrit_min) break
      fstmaxp <- which(cormat == maxcor)
      scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
      df <- df[-fstmaxp]
    }
  }
  scls
}

#' @rdname workhorses
#'
#' @description \code{disj_nci()} is almost identical to \code{disj_pci()},
#'   though step (4) varies slightly from above. To take negative correlations
#'   into account, \code{disj_nci()} flavors the scale with appropriate item as
#'   long as the \emph{absolute} value of the correlation between the sum-score
#'   and a remaining items in the data frame is greater than \code{mrit_min}.
#'
#' @importFrom stats cor
disj_nci <- function(df, mrit_min, sclvals, use) {
  scls <- list()
  msg <- list()
  while (ncol(df) >= 2) {
    scls_len <- length(scls)
    cormat <- cor(df, use = use)
    maxcor <- max(cormat[cormat < 1])
    if (maxcor < mrit_min) break
    fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
    scls[[scls_len + 1]] <- df[fstmaxp]
    df <- df[-fstmaxp]
    while (ncol(df) >= 1) {
      scls_len <- length(scls)
      cormat <- cor(rowSums(scls[[scls_len]]), df, use = use)
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < mrit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        scls[[scls_len]] <- cbind(scls[[scls_len]], df[fstmaxp])
      }else{
        var_rev <- rvrs_var(var = df[fstmaxp], sclvals)
        msg <- c(msg, names(var_rev)) ###
        scls[[scls_len]] <- cbind(scls[[scls_len]], var_rev)
      }
      df <- df[-fstmaxp]
    }
  }
  rvrs_note(msg, applicant = "disjoint")
  scls
}

#' @rdname workhorses
#'
#' @description \code{ovlp_pci()} takes a disjointedly built scale fragment and
#'   tries to extend it with those items in the data set, which are not yet
#'   built into the fragment (aka., its counterpart). Because \code{ovlp_pci()}
#'   does this for every disjointedly built scale fragment it is a multiple
#'   one-dimensional extension of \code{disj_pci()}.
#'
#' @importFrom stats cor
ovlp_pci <- function(msdf, mrit_min, overlap_with, use) {

  # Function ----------------------------------------------------------------

  one_ovlp_pci <- function(scl, ebscl, mrit_min, use) {
    while (ncol(ebscl) >= 1) {
      cormat <- cor(rowSums(scl), ebscl, use = use)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < mrit_min) break
      fstmaxp <- which(cormat == maxcor)
      scl <- cbind(scl, ebscl[fstmaxp])
      ebscl <- ebscl[-fstmaxp]
    }
    scl
  }

  # Pre-set's ---------------------------------------------------------------

  df <- eval(attr(msdf, "df"))

  # Procedure & Options -----------------------------------------------------

  switch(overlap_with,
         fragment = {
           scl_nms <- lapply(msdf, names)
           ebscls <- lapply(scl_nms, extreb_itms, df = df)
         },
         core = {
           msdf <- lapply(msdf, extr_core)
           core_nms <- lapply(msdf, extr_core_nms)
           ebscls <- lapply(core_nms, extreb_itms, df = df)
         },
         stop("Unknown overlapping method. Use either `fragment` or `core`",
              call. = FALSE)
  )
  Map(one_ovlp_pci, msdf, ebscls, MoreArgs = list(mrit_min, use))
}

#' @rdname workhorses
#'
#' @description The only difference to \code{ovlp_pci()} is that
#'   \code{ovlp_nci()} can handle reversed items. The extension algorithm
#'   remains almost the same; \code{ovlp_nci()} flavors \emph{each} scale
#'   fragment with appropriate items from its counterpart as long as the
#'   \emph{absolute} value of the correlation between the sum-score and a
#'   remaining item is greater than \code{mrit_min}. Thus, it is a multiple
#'   one-dimensional extension of \code{disj_nci()}:
#'
#' @importFrom stats cor
ovlp_nci <- function(msdf, mrit_min, overlap_with, sclvals, use) {
  msg <- list()

  # Function ----------------------------------------------------------------

  one_ovlp_nci <- function(scl, ebscl, mrit_min, sclvals, use) {
    while (ncol(ebscl) >= 1) {
      # use = use: Make sure it's an ARG
      cormat <- cor(rowSums(scl), ebscl, use = use)
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < mrit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        scl <- cbind(scl, ebscl[fstmaxp])
      }else{
        var_rev <- rvrs_var(var = ebscl[fstmaxp], sclvals)
        assign("msg", c(msg, names(var_rev)), envir = parent.frame(3))
        scl <- cbind(scl, var_rev)
      }
      ebscl <- ebscl[-fstmaxp]
    }
    scl
  }

  # Pre-Set's ---------------------------------------------------------------

  df <- eval(attr(msdf, "df"))

  # Procedure & Options -----------------------------------------------------

  switch(overlap_with,
         fragment = {
           scl_nms <- lapply(msdf, names)
           ebscls <- lapply(scl_nms, extreb_itms, df = df)
         },
         core = {
           msdf <- lapply(msdf, extr_core)
           core_nms <- lapply(msdf, extr_core_nms)
           ebscls <- lapply(core_nms, extreb_itms, df = df)
         },
         stop("Unknown overlapping method. Use either `fragment` or `core`",
              call. = FALSE)
  )
  res <- Map(one_ovlp_nci, msdf, ebscls,
             MoreArgs = list(mrit_min, sclvals, use))
  rvrs_note(msg, applicant = "overlap")
  res
}
