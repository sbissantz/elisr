
# disjoint ----------------------------------------------------------------

disjoint <- function(df = NULL, rit_min = .3, negative_too = FALSE,
                     sclvals = NULL, ...) {
  stopifnot(all(hasName(df, names(df))))
  if (isTRUE(negative_too)){
   lodis <- disunito(df, rit_min, sclvals)
   new_muscldf(lodis, df = match.call()$df, method = "disjoint",
               rit_min = match.call()$rit_min, negative_too = TRUE,
               sclvals = match.call()$sclvals)
   }else{
   lodis <- disupi(df, rit_min)
   new_muscldf(lodis, df = match.call()$df, method = "disjoint",
               rit_min = match.call()$rit_min, negative_too = FALSE,
               sclvals = match.call()$sclvals)
  }
}
