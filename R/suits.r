suits <- function(df, reuse = TRUE) {
  reuse_logi <- is.logical(reuse)
  if (isFALSE(reuse_logi))
    stop("`reuse` must be logical (i.e., either TRUE or FALSE)", call. = FALSE)
  tryCatch(check_df(df),
           error = function(e) {
             cmsg <- conditionMessage(e)
             advice <- "Scaling not possible!"
             hint <- "\n Hint: Fix this error and retry. \n"
             errmsg <- paste(advice, cmsg, hint)
             stop(errmsg, call. = FALSE)
           },
           warning = function(w) {
             cmsg <- conditionMessage(w)
             advice <- "Scaling not recommended \n"
             hint <- "Hint: Investigate in this warning and retry. \n"
             wrnmsg <- paste(cmsg, advice, hint)
             warning(wrnmsg, call. = FALSE)
           }
  )
  df_nme <- match.call()$df
  msg <- paste0("`", df_nme, "`", " fits the bill. Ready to scale! ")
  message(msg)

  if (reuse) {
    df_str <- structure(df, pre_checked = TRUE)
    invisible(df_str)
  }
}
#
# Attaches an Attr  "checks = TRUE" and messages if data.frame is not checked?
#
