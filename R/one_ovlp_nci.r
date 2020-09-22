# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
# One Overlap -- Negative Correlating Items -------------------------------

one_ovlp_nci <- function(scl, ebscl, rit_min, ...) {
  while (ncol(ebscl) >= 1) {
    cormat <- cor(rowSums(scl), ebscl, ...)
    maxcor <- max(abs(cormat[cormat < 1]))
    if (maxcor < rit_min) break
    fstmaxp <- which(abs(cormat) == maxcor)
    corsign <- sign(cormat[fstmaxp])
     if (corsign >= 0) {
       scl <- cbind(scl, ebscl[fstmaxp])
       }else{
         var_rev <- rvrs_var(var = ebscl[fstmaxp], sclvals)
         scl <- cbind(scl, var_rev)
         }
    ebscl <- ebscl[-fstmaxp]
    }
  scl
}
