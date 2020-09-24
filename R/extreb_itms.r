
# Extract Everything But Items --------------------------------------------

extreb_itms <- function(df, itm_nms) {
  df[, -which(names(df) %in% itm_nms)]
}
