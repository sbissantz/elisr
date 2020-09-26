
# Overlapping Scaling Process Using Positive Items ------------------------

# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"

ovlp_pci <- function(muscldf, rit_min = NULL, overlap_with = NULL, ...) {

# pre-defined values -----------------------------------------------------

  df <- eval(attr(muscldf, "df"))
  scl_nms <- lapply(muscldf, names)
  core_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# options -----------------------------------------------------------------

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
