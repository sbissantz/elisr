# TODO Use "!"for not, is FALSE for conditions
# TODO Check if there is a linear relationship between the variables?
# TODO WARNING should be errors?

check_my <- function(x){
  if(!is.data.frame(x)){
    stop("`x` is not a data frame. Scaling not possible", call. = FALSE)
    }else{
      message("`x` is a data frame.")
      }
  x_len <- length(x)
  if(x_len < 2){
    stop("`x` has `< 2` variables. Sclaing not possible.", call. = FALSE)
  }else{
    if(x_len >= 2 && x_len <= 3){
      warning("`x` has  `< 4`. Scaling possible but not recommended.",
              call. = FALSE)
    }else{
      if(x_len > 3){
        msg <- paste("`x` has", x_len, "variables")
        message(msg)
      }
    }
  }
  x_nms <- names(x)
  x_unms <- unique(x_nms)
  if(is.null(names)){
    stop("`x` has no (col)names. Scaling not possible.", call. = FALSE)
  }else{
    if(length(x_nms) > length(x_unms)){
    warning ("`x` names are not unique. Scaling possible but not recommended.",
             call. = FALSE)
    }else{
      message("`x` names are unique.")
    }
    if(anyNA(x_nms)){
      warning ("`x` has `NA` names. Scaling possible but not recommended.",
               call. = FALSE)
    }else{
      message("`x` has no `NA` names.")
    }
  }
  message("Ready to go!")
}
