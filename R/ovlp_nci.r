
# Overlapping Scaling Process Using Negative Items Too --------------------
# TODO muscldf -> muscls
# TODO check `...`
# TODO if someone hasnt disunito() before he hasn't specified a sclvals

ovlp_nci <- function(muscldf, rit_min = NULL, overlap_with = NULL,
                     sclvals = NULL, ...) {

  df_nme <- attr(muscldf, "df")
  df <- eval(df_nme)
  if(is.null(rit_min)){
    rit_min <- attr(muscldf, "rit_min")
  }
  check_rit(rit_min)

  if(is.null(sclvals)){
  sclvals <- attr(muscldf, "sclvals")
  }
  check_sclvals(sclvals)

  scl_nms <- lapply(muscldf, names)
  core_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])
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
