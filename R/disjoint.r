
# disjoint ----------------------------------------------------------------

disjoint <- function(df, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, ...) {
  if (isTRUE(negative_too)){
    scls <- disunito(df, rit_min, sclvals)
    new_muscldf(scls, df = match.call()$df, method = "disjoint",
                rit_min = rit_min, negative_too = TRUE, sclvals = sclvals)
    }else{
      scls <- disj_pci(df, rit_min)
      new_muscldf(scls, df = match.call()$df, method = "disjoint",
               rit_min = rit_min, negative_too = FALSE, sclvals = sclvals)
  }
}
