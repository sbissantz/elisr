#' @title Multiple Scaling In An Overlapping Manner Using Positive Items

#' @description \code{ovlp_pci} is an internal function. It returns a list of
#'   data frames by taking one (\code{muscldf}). In order to do that,
#'   \code{ovlp_pci} pre-sets the scales by using either the core items of the
#'   given multiple scaled data frame or the full scale, i.e. all items of each
#'   scale (\code{scls}). Furthermore, \code{ovlp_nci} pre-sets a list of scales
#'   including everything but these items (\code{ebscls}). These steps equal the
#'   building process (step 1 to 3) in the disjoint scaling procedure. Thus, the
#'   given scale need to be expanded. Therefore, \code{ovlp_pci} uses each
#'   component of the second list (\code{ebscls}) on a step by step base. In a
#'   particular case this happens as follows: While the correlation coefficient
#'   of a remaining item in the scale (with the sum score) is greater than the
#'   pre-specified lower bound, the scale is enlarged with the according item
#'   (from \code{ebscls}). After one overlap \code{ovlp_pci} moves on to the
#'   next scale.
#'
#' @param muscldf a multiple scaled data frame (built with \code{disjoint}).
#'
#' @param rit_min a numerical constant to set the (corrected item total)
#'   correlation. The value of this lower bound must be in the range of 0 to 1.
#'   If no value is entered (\code{NULL}, the default) \code{overlap} tries to
#'   get the one specified in \code{disjoint}, by looking for a \code{rit_min}
#'   attribute in the given \code{muscldf}.
#'
#' @param overlap_with a string telling \code{overlap} which items to start the
#'   scaling process with. One can choose to use either the "core" of each scale
#'   or the "full_scale". The default is set to "full_scale".
#'
#' @param use an optional string indicating how to deal with missing values if
#'   necessary. See \code{use} in \code{\link[stats]{cor}} for details.
#'
#' @details The \code{use} argument takes control over the treatment of missing
#'   values when correlation matrices are build. In a scaling process this
#'   happens at least twice: first when determining the core (the two items of
#'   the correlation matrix with the highest linear relationship), and second
#'   when an item is considered to be part of this scale.
#'
#'   Hint: If there ought to be two items in the scaling process, having an
#'   equal correlation, e.g. with the sum score, always the first one is used.
#'
#' @references Müller-Schneider, Thomas. (2001). Multiple Skalierung nach dem
#'   Kristallisationsprinzip / Multiple Scaling According to the Principle of
#'   Crystallization. Zeitschrift für Soziologie. 30. 10.1515/zfsoz-2001-0404.

ovlp_pci <- function(muscldf, rit_min = NULL, overlap_with = NULL,
                     use) {

# Function ----------------------------------------------------------------

one_ovlp_pci <- function(scl, ebscl, rit_min, use) {
   while (ncol(ebscl) >= 1) {
     cormat <- cor(rowSums(scl), ebscl, use = use)
     maxcor <- max(cormat[cormat < 1])
     if (maxcor < rit_min) break
     fstmaxp <- which(cormat == maxcor)
     scl <- cbind(scl, ebscl[fstmaxp])
     ebscl <- ebscl[-fstmaxp]
     }
   scl
 }

# Pre-Set's ---------------------------------------------------------------

df <- eval(attr(muscldf, "df"))
scl_nms <- lapply(muscldf, names)
core_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# Procedure & Options -----------------------------------------------------

switch(overlap_with,
       full_scale = {
         scls <- lapply(scl_nms, extr_itms, df = df)
         ebscls <- lapply(scl_nms, extreb_itms, df = df)
         },
       score = {
         scls <- lapply(core_nms, extr_itms, df = df)
         ebscls <- lapply(core_nms, extreb_itms, df = df)
         },
       stop("Unknown overlapping method. Use either `core` or `full_scale`",
            call. = FALSE)
       )
Map(one_ovlp_pci, scls, ebscls, MoreArgs = list(rit_min, use))
}
