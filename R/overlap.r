# muscaovl ------------------------------------------------------------------
# Multiple Scaling in an overlapping manner
# TODO 'stopifnot()' Objekt is of 'class()' 'scldf'
# Use:inherits(scldf, "muscat)
# TODO use attribute 'rit.min' (?)
# TODO
# Should be caught 'by attribute' (?)
# TODO Make senese: first scaling with negatives, then overlap without?
# TODO: Include negative items in the overlap...
# Assure colnames/names from scld otherwise: 'colnames got lost on the way'

disjoint <- function( df, rit_min = .3, negative_too = FALSE,
full_scl_val = NULL,  overlap_with = NULL, ...) {
  stopifnot(is.data.frame(df))
# Should be included with disjoint()
#  if (is.null(names(df)))
#    stop("No colnames found. Please specify them.", call. = FALSE)
  if (isTRUE(negative_too) && is.null(full_scl_val))
    stop("No full scale value found. Please specify one.", call. = FALSE)
  if (isTRUE(overlap)  && is.null(overlap_with))
    stop("No method to overlap with. Please specify one.", call. = FALSE)

  # wfls which includes the variables to work with and
  # and ovls which hoovers the information from the scale

  # TODO cant include negative items?!?!
  scl_ovlp <- function(ovls, wfls) {
    if (isFALSE(negative_too)) {
      while (ncol(df) >= 1) {
        uni_len <- length(scldf)
        cormat <- cor(rowSums(scldf[[uni_len]]), df)
        maxcor <- max(cormat[cormat < 1])
        if (maxcor < rit_min) break
        fstmaxp <- which(cormat == maxcor)
        scldf[[uni_len]] <- cbind(scldf[[uni_len]], df[fstmaxp])
        df <- df[-fstmaxp]
        }
      }else{
        while (ncol(df) >= 1) {
          uni_len <- length(scldf)
          cormat <- cor(rowSums(scldf[[uni_len]]), df)
          maxcor <- max(abs(cormat[cormat < 1]))
          if (maxcor < rit_min) break
          fstmaxp <- which(abs(cormat) == maxcor)
          corsign <- sign(cormat[fstmaxp])
          if (corsign >= 0) {
            scldf[[uni_len]] <- cbind(scldf[[uni_len]], df[fstmaxp])
            }else{
              var_rev <- (full_scl_val + 1) - df[fstmaxp]
              scldf[[uni_len]] <- cbind(scldf[[uni_len]], var_rev)
              }



    return(ovls)
  }
    scl_nms <- lapply(scldf, colnames)
    nuc_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])
    if (overlap_with == "full.scale") {
      ovls <- lapply(scl_nms, function(scl_nms) df[, scl_nms])
      nme_scl <- function(scl_nms) df[, -which(colnames(df) %in% scl_nms)]
      wfls <- lapply(scl_nms, nme_scl)
      }
    if (overlap_with == "nucleus") {
      ovls <- lapply(nuc_nms, function(nuc_nms) df[, nuc_nms])
      nme_nuc <- function(nuc_nms) df[, -which(colnames(df) %in% nuc_nms)]
      wfls <- lapply(nuc_nms, nme_nuc)
      }
    return(Map(scl_ovlp, ovls, wfls))
    }
}
