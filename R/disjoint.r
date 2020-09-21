
# disjoint ----------------------------------------------------------------

disjoint <- function(df, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, ...) {
  if (isTRUE(negative_too)){
    lodis <- disunito(df, rit_min, sclvals)
    new_muscldf(lodis, df = match.call()$df, method = "disjoint",
                rit_min = rit_min, negative_too = TRUE, sclvals = sclvals)
    }else{
      lodis <- disupi(df, rit_min)
      new_muscldf(lodis, df = match.call()$df, method = "disjoint",
               rit_min = rit_min, negative_too = FALSE, sclvals = sclvals)
  }
}