
# Overlapping Scaling Process Using Positive Items ------------------------

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
