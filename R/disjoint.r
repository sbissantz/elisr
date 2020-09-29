
# Multiple Scaing in A Disjoint Manner ------------------------------------

disjoint <- function(df, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, use = "pairwise.complete.obs") {

# Checks ------------------------------------------------------------------

  check_df(df)
  check_rit(rit_min)
  check_neg(negative_too)

# Procedure & Options -----------------------------------------------------

  if (negative_too){
    check_sclvals(sclvals)
    scls <- disj_nci(df, rit_min, sclvals, use)
    new_muscldf(scls, df = match.call()$df, method = "disjoint",
                rit_min = rit_min, negative_too = TRUE, sclvals = sclvals)
    }else{
      scls <- disj_pci(df, rit_min, use)
      new_muscldf(scls, df = match.call()$df, method = "disjoint",
               rit_min = rit_min, negative_too = FALSE)
  }
}

