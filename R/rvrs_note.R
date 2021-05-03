#' @title Notification message for a reversed variable
#'
#' @description \code{rvrs_note()} gets the full report of reversed variables
#'   and reports a unique list of them.
#'
#' @param msg a reverse message sent from either \code{disjoint()} or
#'   \code{overlap()}
#'
#' @param applicant the function which wants to leave messages \code{disjoint()}
#'   or \code{overlap()})
#'
#' @return The function is called for its side effects. It leaves a message when
#'   an item is reversed.
#'
rvrs_note <- function(msg, applicant) {
  msg_len <- length(msg)
  if (msg_len == 0) {
    message(paste0("\n", applicant, "() didn't reverse an item.\n"))
  } else {
    message(
      paste0("\n", applicant, "() reversed the following item(s):\n"),
      paste0("- ", unique(msg), "\n"))
  }
}
