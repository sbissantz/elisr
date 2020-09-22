# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
# One Overlap -- Positive Correlating Items -------------------------------

one_ovlp_pci <- function(scl, ebscl, rit_min, ...) {
   while (ncol(ebscl) >= 1) {
     cormat <- cor(rowSums(scl), ebscl, ...)
     maxcor <- max(cormat[cormat < 1])
     if (maxcor < rit_min) break
     fstmaxp <- which(cormat == maxcor)
     scl <- cbind(scl, ebscl[fstmaxp])
     ebscl <- ebscl[-fstmaxp]
     }
   scl
 }

 #one_ovlp_pci <- function(ovls, wfls, rit_min, ...) {
 #  while (ncol(wfls) >= 1) {
 #    cormat <- cor(rowSums(ovls), wfls, ...)
 #    maxcor <- max(cormat[cormat < 1])
 #    if (maxcor < rit_min) break
 #    fstmaxp <- which(cormat == maxcor)
 #    ovls <- cbind(ovls, wfls[fstmaxp])
 #    wfls <- wfls[-fstmaxp]
 #    }
 #  return(ovls)
 #}

