suits <- function(df) {
  tryCatch(check_my(df),
           error = function(e) {
             #errmsg <- conditionMessage(e)
             errmsg <- "Scaling not possible"
             stop(errmsg, call. = FALSE)
             }
  )
  }


