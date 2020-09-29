#' Multiple scaling in a disjoint manner
#'
#' @param df a data frame.
#' @param rit_min a corrected item total correlation between 0 and 1 marking the
#'   lower bound of the scaling process.
#' @param negative_too a logical constant indicating whether negatively
#'   correlating items should be included in the scaling process.
#' @param sclvals a double vector of the form \code{c(min,max)} marking the
#'   start and end point of your scale, e.g.
#'

disjoint <- function(df, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, use = "pairwise.complete.obs") {

# Checks ------------------------------------------------------------------

  check_df(df)
  check_rit(rit_min)
  check_neg(negative_too)

# Procedure & Options -----------------------------------------------------

  if (negative_too) {
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
