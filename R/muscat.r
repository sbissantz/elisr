# muscat ------------------------------------------------------------------

muscat <- function(
  df,
  rit_min = .3,
  overlap = FALSE,
  negative_too = FALSE,
  full_scl_val = NULL,
  overlap_with = NULL,
  pre_scldf = NULL, ...) {
  stopifnot(is.data.frame(df))
  if (is.null(names(df)))
    stop("No colnames found. Please specify them.", call. = FALSE)
  if (isTRUE(negative_too) && is.null(full_scl_val))
    stop("No full scale value found. Please specify one.", call. = FALSE)
  if (isTRUE(overlap)  && is.null(overlap_with))
    stop("No method to overlap with. Please specify one.", call. = FALSE)
  # scl_dsjnt creates a disjoint(1) scldf
  # -- according to the principle of# cristallization

# scl_dsjt ----------------------------------------------------------------

  scl_dsjt <- function(df, rit_min, negative_too, full_scl_val, ...) {
    scldf <- list()
    while (ncol(df) >= 2) {
      uni_len <- length(scldf)
      # Specify which values to use "complete","pairwise"
      cormat <- cor(df, ...)
      maxcor <- max(cormat[cormat < 1])
      if (maxcor < rit_min) break
      # Take the first(!) maximum
      fstmaxp <- which(cormat == maxcor, arr.ind = TRUE)[1, ]
      scldf[[uni_len + 1]] <- df[fstmaxp]
      df <- df[-fstmaxp]
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
            df <- df[-fstmaxp]
          }
        }
      }
    return(scldf)
  }

# scl_ovlp ----------------------------------------------------------------

  # scl_ovlp does overlapping scale building using a pre-build
  # wfls which includes the variables to work with and
  # and ovls which hoovers the information from the scale
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

# Output ------------------------------------------------------------------

  # If smo wants only a disjoint scale building procedure
  if (isFALSE(overlap)) {
    scl_dsjt(df, rit_min, negative_too, full_scl_val)
    }else{
      # Given a prebuild disjoint scale, put the overlap on top. If not..
      # If not, start the process from scratch.
      if (is.null(pre_scldf)) {
      scldf <- scl_dsjt(df, rit_min, negative_too, full_scl_val)
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
