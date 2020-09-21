
# Overlapping Scaling Process Using Negative Items Too --------------------

# ovlunito takes a multiple sclaed data.frame, a lower bound in form of a
# rit_min, and a fullscale value. By choosing the items used in the overlapping
# process (only the highest correlating pair of each scale, i.e. the core or
# trying to overlap or expand the 'full_scale'). No matter But whatever which
# one one chooses it returns again a (list of) multiple scaled data.frame(s).
# Ellipsis is set  to allow for any na.action(), Hint: One could also set the
# 'method' argument from cor() but this is not tested yet.

ovlunito <- function(muscldf, rit_min = NULL, overlap_with = NULL,
                     sclvals = NULL, ...) {

  if (isFALSE(inherits(muscldf, "muscldf")))
    stop("This is not a muscldf. Please build one.", call. = FALSE)
  if (is.null(overlap_with))
    stop("No method to 'overlap_with'. Please specify one.", call. = FALSE)
#  if (is.null(sclvals))
#    stop("No full scale value found. Please specify one.", call. = FALSE)
# TODO: if overlap with is different from core, full_scale, error
# units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  # stopifnot(attr(muscldf, "colnames"))

# TODO, check if everywhere is "..."

# functions ---------------------------------------------------------------

  scl_ovlp <- function(ovls, wfls, ...) {
    while (ncol(wfls) >= 1) {
      cormat <- cor(rowSums(ovls), wfls, ...)
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < rit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        ovls <- cbind(ovls, wfls[fstmaxp])
        }else{
          var_rev <- rvrs_var(var = wfls[fstmaxp], sclvals)
          ovls <- cbind(ovls, var_rev)
        }
      wfls <- wfls[-fstmaxp]
      }
    return(ovls)
  }

    #TODO if someone hasnt disunito() before he hasn't specified a sclvals

# pre-defined values -----------------------------------------------------

  df <- eval(attr(muscldf, "df"))
  if (is.null(rit_min)) {
    rit_min <- attr(muscldf, "rit_min")
  }
  sclvals <- attr(muscldf, "sclvals")
  scl_nms <- lapply(muscldf, names)
  nuc_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# options -----------------------------------------------------------------

    if (overlap_with == "full_scale") {
    ovls <- lapply(scl_nms, function(scl_nms) df[, scl_nms])
    nme_scl <- function(scl_nms) df[, -which(names(df) %in% scl_nms)]
    wfls <- lapply(scl_nms, nme_scl)
    }
    if (overlap_with == "core") {
    (ovls <- lapply(nuc_nms, function(nuc_nms) df[, nuc_nms]))
    nme_nuc <- function(nuc_nms) df[, -which(names(df) %in% nuc_nms)]
    (wfls <- lapply(nuc_nms, nme_nuc))
 }

# lovls -------------------------------------------------------------------

  # List of overlapping scales
  # lovls <- Map(scl_ovlp, ovls, wfls)
  Map(scl_ovlp, ovls, wfls)

# attributes --------------------------------------------------------------

}
