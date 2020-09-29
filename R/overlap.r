
# Multiple Scaling In An Overlapping Manner -------------------------------

overlap <- function(muscldf, rit_min = NULL, negative_too = FALSE,
                    overlap_with = "full_scale", sclvals = NULL,
                    use = "pairwise.complete.obs") {

# Checks ------------------------------------------------------------------

  check_neg(negative_too)
  check_muscldf(muscldf)
  if(is.null(rit_min)){
    rit_min <- attr(muscldf, "rit_min")
    }
  check_rit(rit_min)
  check_ovlp(overlap_with)

# Procedure & Options -----------------------------------------------------

  if(negative_too){
    sclvals_attr <- attr(muscldf, "sclvals")
    compare_sclvals(sclvals, sclvals_attr)
    if(is.null(sclvals)){
      sclvals <- sclvals_attr
    }
    check_sclvals(sclvals)
    scls <- ovlp_nci(muscldf, rit_min, overlap_with, sclvals, use = use)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = TRUE,
                sclvals = sclvals)
  }else{
    scls <- ovlp_pci(muscldf, rit_min, overlap_with, use = use)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = FALSE)
  }
}
