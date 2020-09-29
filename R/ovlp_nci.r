
# Overlapping Scaling Process Using Negative Items Too --------------------

ovlp_nci <- function(muscldf, rit_min, overlap_with, sclvals,
                     use) {

# Function ----------------------------------------------------------------

one_ovlp_nci <- function(scl, ebscl, rit_min, sclvals, use) {
  while (ncol(ebscl) >= 1) {
    # use = use: Make sure it's an ARG
    cormat <- cor(rowSums(scl), ebscl, use = use)
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
       core = {
       scls <- lapply(core_nms, extr_itms, df = df)
       ebscls <- lapply(core_nms, extreb_itms, df = df)
       },
       stop("Unknown overlapping method. Use either `core` or `full_scale`",
            call. = FALSE)
       )
# Map(one_ovlp_nci, scls, ebscls, rit_min)
Map(one_ovlp_nci, scls, ebscls, MoreArgs = list(rit_min, sclvals, use))
}
