# TODO muscldf -> muscls
# TODO check `...`
# TODO check that all "''" are "``"
# TODO mak overlap_with -> method, olap_method
# TODO negative_too -> change
# TODO stopifnot( negative_too) is either TRUE or FALSE, i.e. exclude NULL (input must be logical)

overlap <- function(muscldf, rit_min = .3, negative_too = FALSE,
                    overlap_with = "full_scale", sclvals = NULL, ...){

  if (isFALSE(inherits(muscldf, "muscldf")))
    stop("`muscldf` is not of its type. Consider `disjoint()` to create one.",
         call. = FALSE)
  if (is.null(overlap_with))
    stop("No method to 'overlap_with'. Please specify one.", call. = FALSE)
  if(negative_too){
    # check_sclvals(sclvals)
    scls <- ovlp_nci(muscldf, rit_min, overlap_with, sclvals, ...)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = TRUE, sclvals = attr(muscldf, "sclvals"))
  }else{
    scls <- ovlp_pci(muscldf, rit_min, overlap_with, sclvals, ...)
    new_muscldf(scls, df = attr(muscldf, "df"), method = "overlap",
                rit_min = rit_min, negative_too = FALSE)
  }
}

