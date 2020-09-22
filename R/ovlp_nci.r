
# Overlapping Scaling Process Using Negative Items Too --------------------
# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
# TODO if someone hasnt disunito() before he hasn't specified a sclvals

# ovlunito takes a multiple sclaed data.frame, a lower bound in form of a
# rit_min, and a fullscale value. By choosing the items used in the overlapping
# process (only the highest correlating pair of each scale, i.e. the core or
# trying to overlap or expand the 'full_scale'). No matter But whatever which
# one one chooses it returns again a (list of) multiple scaled data.frame(s).
# Ellipsis is set  to allow for any na.action(), Hint: One could also set the
# 'method' argument from cor() but this is not tested yet.

ovlp_nci <- function(muscldf, rit_min = NULL, overlap_with = NULL,
                     sclvals = NULL, ...) {

  if (isFALSE(inherits(muscldf, "muscldf")))
    stop("This is not a muscldf. Please build one.", call. = FALSE)
  if (is.null(overlap_with))
    stop("No method to 'overlap_with'. Please specify one.", call. = FALSE)
#  if (is.null(sclvals))
#    stop("No full scale value found. Please specify one.", call. = FALSE)

  # stopifnot(attr(muscldf, "colnames"))

  df <- eval(attr(muscldf, "df"))
  if (is.null(rit_min)) {
    rit_min <- attr(muscldf, "rit_min")
  }
  sclvals <- attr(muscldf, "sclvals")
  scl_nms <- lapply(muscldf, names)
  core_nms <- lapply(scl_nms, function(scl_nms) scl_nms[c(1, 2)])

# options -----------------------------------------------------------------

  switch(overlap_with,
         full_scale = {
          scls <- Map(extr_itms, muscldf, scl_nms)
          ebscls <- Map(extreb_itms, muscldf, scl_nms)
         },
         core = {
          scls <- Map(extr_itms, muscldf, core_nms)
          ebscls <- Map(extreb_itms, muscldf, core_nms)
         },
         stop("Unknown overlapping method. Use either `core` or `full_scale`",
              call. = FALSE)
         )
  Map(one_ovlp_nci, scls, ebscls, rit_min)
}
