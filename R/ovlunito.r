# Overlapping Scaling Process Using Negative Items Too --------------------

ovlunito <- function(muscldf, overlap_with = NULL, rit_min = NULL,
                     fullscl_val = NULL, ...) {

# checks ------------------------------------------------------------------

  stopifnot(attr(muscldf,"colnames"))
  if(isFALSE(inherits(muscldf, "muscldf")))
    stop("This is not a multiple scaled data frame. Please build on")
  if (is.null(overlap_with))
    stop("No method to 'overlap_with'. Please specify one.")

# functions ---------------------------------------------------------------

  scl_ovlp <- function(ovls, wfls) {
    while (ncol(df) >= 1) {
      cormat <- cor(rowSums(ovls, wfls))
      maxcor <- max(abs(cormat[cormat < 1]))
      if (maxcor < rit_min) break
      fstmaxp <- which(abs(cormat) == maxcor)
      corsign <- sign(cormat[fstmaxp])
      if (corsign >= 0) {
        ovls <- cbind(ovls, wfls[fstmaxp])
        }else{
          var_rev <- (fullscl_val + 1) - wfls[fstmaxp]
          ovls <- cbind(ovls, var_rev)
          }
      df <- df[-fstmaxp]
    }
    return(ovls)
  }

# pre-defined values -----------------------------------------------------

  df <- eval(attr(muscldf, "df"))
  if (is.null(rit_min)){
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

  structure(lovls, class = "muscldf", rit_min = rit_min, df = match.call()$df)
}
