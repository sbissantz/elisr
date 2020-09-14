
# Overlapping Scaling Process Using Positive Items ------------------------

# ovlupi takes a multiple sclaed data.frame and a lower bound in form of a
# rit_min. By choosing the items used in the overlapping process (using only the
# highest correlating pair of each scale, i.e. the core, or trying to overlap
# or expand the 'full_scale'). No matter But whatever which one one chooses it
# returns again a (list of) multiple scaled data.frame(s). Ellipsis is set to
# allow for any na.action(), Hint: One could also set the 'method' argument from
# cor() but this is not tested yet.

ovlupi <- function(muscldf, rit_min = NULL, overlap_with = NULL, ...) {

# checks ------------------------------------------------------------------

  if (isFALSE(inherits(muscldf, "muscldf")))
    stop("This is not a muscldf. Please build on", call. = FALSE)
  if (isFALSE(attr(muscldf, "colnames")))
    stop("Colnames seemd to be lost. Please search for them.", call. = FALSE)
  if (is.null(overlap_with))
    stop("No method to 'overlap_with'. Please specify one.", call. = FALSE)
  # TODO: if overlap with is different from core, full_scale, error
  #units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))

# functions ---------------------------------------------------------------

  scl_ovlp <- function(ovls, wfls) {
    while (ncol(wfls) >= 1) {
      cormat <- cor(rowSums(ovls), wfls, ...)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < rit_min) break
      fstmaxp <- which(cormat == maxcor)
      ovls <- cbind(ovls, wfls[fstmaxp])
      wfls <- wfls[-fstmaxp]
      }
    return(ovls)
  }

# pre-defined values -----------------------------------------------------

  df <- eval(attr(muscldf, "df"))
  if (is.null(rit_min)) {
    rit_min <- attr(muscldf, "rit_min")
  }
  scl_nms <- lapply(muscldf, names)
  nuc_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# options -----------------------------------------------------------------

  if (overlap_with == "full_scale") {
    ovls <- lapply(scl_nms, function(scl_nms) df[, scl_nms])
    nme_scl <- function(scl_nms) df[, -which(names(df) %in% scl_nms)]
    wfls <- lapply(scl_nms, nme_scl)
    }
  if (overlap_with == "core") {
    ovls <- lapply(nuc_nms, function(nuc_nms) df[, nuc_nms])
    nme_nuc <- function(nuc_nms) df[, -which(names(df) %in% nuc_nms)]
    wfls <- lapply(nuc_nms, nme_nuc)
  }

# lovls -------------------------------------------------------------------

  # List of overlapping scales
  lovls <- Map(scl_ovlp, ovls, wfls)

# attributes --------------------------------------------------------------

  structure(lovls, class = "muscldf", scl_method = "overlap",
            rit_min = rit_min, df = match.call()$df)
}
