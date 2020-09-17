suits <- function(df) {
  tryCatch(check_my(df),
           error = function(e) {
             cmsg <- conditionMessage(e)
             advice <- "Scaling not possible!"
             hint <- "\n Hint: Fix this error and retry. \n"
             errmsg <- paste(advice, cmsg, hint)
              stop(errmsg, call. = FALSE)
           },
           warning = function(w){
             cmsg <- conditionMessage(w)
             advice <- "Scaling not recommended \n"
             hint <- "Hint: Investigate in this warning and retry. \n"
             wrnmsg <- paste(cmsg, advice, hint)
              warning(wrnmsg, call. = FALSE)
           }
        )
  invisible(structure(df, checks = TRUE))
  }

#
# Attaches an Attr  "checks = TRUE" and messages if data.frame is not checked?
#
