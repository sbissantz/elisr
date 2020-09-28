# TODO muscldf -> muscls
# TODO check `...`

# disjoint ----------------------------------------------------------------

disjoint <- function(df, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, ...) {

# Checks ------------------------------------------------------------------

  check_neg(negative_too)
  check_df(df)
  check_rit(rit_min)

# Procedures --------------------------------------------------------------

  if (negative_too){
    check_sclvals(sclvals)
    scls <- disj_nci(df, rit_min, sclvals)
    new_muscldf(scls, df = match.call()$df, method = "disjoint",
                rit_min = rit_min, negative_too = TRUE, sclvals = sclvals)
    }else{
      scls <- disj_pci(df, rit_min)
      new_muscldf(scls, df = match.call()$df, method = "disjoint",
               rit_min = rit_min, negative_too = FALSE)
  }
}

