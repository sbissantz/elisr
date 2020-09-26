
# Overlapping Scaling Process Using Negative Items Too --------------------

# TODO check `...`

ovlp_nci <- function(muscldf, rit_min = NULL, overlap_with = NULL,
                     sclvals = NULL, ...) {
# Pre-Sets ----------------------------------------------------------------

  df <- eval(attr(muscldf, "df"))
  scl_nms <- lapply(muscldf, names)
  core_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# Options -----------------------------------------------------------------

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
  Map(one_ovlp_pci, scls, ebscls, rit_min)
}
