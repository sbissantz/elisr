# -------------------------------------------------------------------------

# TODO check `...`
# TODO muscldf -> muscls
# TODO mak overlap_with -> method, olap_method
# TODO negative_too -> change

overlap <- function(muscldf, rit_min = .3, negative_too = FALSE,
                    overlap_with = "full_scale", sclvals = NULL, ...) {

# Checks ------------------------------------------------------------------

  check_neg(negative_too)
  check_muscldf(muscldf)
  check_rit(rit_min)
  check_neg(negative_too)
  check_ovlp(overlap_with)

# Procedures --------------------------------------------------------------

  if(negative_too){
    check_sclvals(sclvals)
    scls <- ovlp_nci(muscldf, rit_min, overlap_with, sclvals, ...)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = TRUE,
                sclvals = attr(muscldf, "sclvals"))
  }else{
    scls <- ovlp_pci(muscldf, rit_min, overlap_with, sclvals, ...)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = FALSE)
  }
}
