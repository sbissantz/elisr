# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
# Overlapping Scaling Process Using Positive Items ------------------------

# ovlupi takes a multiple sclaed data.frame and a lower bound in form of a
# rit_min. By choosing the items used in the overlapping process (using only the
# highest correlating pair of each scale, i.e. the core, or trying to overlap
# or expand the 'full_scale'). No matter But whatever which one one chooses it
# returns again a (list of) multiple scaled data.frame(s). Ellipsis is set to
# allow for any na.action(), Hint: One could also set the 'method' argument from
# cor() but this is not tested yet.

ovlp_pci <- function(muscldf, rit_min = NULL, overlap_with = NULL, ...) {

# checks ------------------------------------------------------------------

  if (isFALSE(inherits(muscldf, "muscldf")))
    stop("This is not a muscldf. Please build on", call. = FALSE)

  if (is.null(overlap_with))
    stop("No method to `overlap_with`. Please specify one.", call. = FALSE)
  #units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  #if (isFALSE(attr(muscldf, "colnames")))
  #  stop("Colnames seemd to be lost. Please search for them.", call. = FALSE)

# pre-defined values -----------------------------------------------------

  df <- eval(attr(muscldf, "df"))
  if (is.null(rit_min)) {
    rit_min <- attr(muscldf, "rit_min")
  }
  check_rit(rit_min)
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
