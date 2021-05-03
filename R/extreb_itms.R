#' @title Extract everything but the core
#'
#' @description \code{extreb_itms()} builds the counterpart of a fragment from
#'   the given item names. Therefore, the counterpart includes all variables
#'   that are not part of a fragment but which are mentioned in the specified
#'   data set.
#'
#' @param df a data frame object.
#'
#' @param itm_nms the names of an item from a scale.
#'
#' @return A numeric vector of length m minus two, where 'm' specifies the
#'   number of variables in a given data set. It contains the items of a
#'   scale's counterpart.
#'
extreb_itms <- function(df, itm_nms) {
  df[-which(names(df) %in% itm_nms)]
}
