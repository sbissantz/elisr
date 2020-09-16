
# Suit --------------------------------------------------------------------

# TODO Use "!"for not, is FALSE for conditions

suits <- function(x){
  if(!is.data.frame(x)){
    stop("`x` is not a data frame. Scaling not possible", call. = FALSE)
    }else{
      cat("`x` is a data frame.", "\n")
      }
  x_len <- length(x)
  if(x_len < 2){
    stop("`x` has `>=1` variable. Sclaing not possible.", call. = FALSE)
  }else{
    if(x_len >= 2 && x_len <= 3){
      warning("`x` has  only `>= 3`. Scaling possible but not recommended.",
              call. = FALSE)
    }else{
      if(x_len > 3){
        cat("`x` has", x_len, "variables", "\n")
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
      cat("`x` names are unique.", "\n")
    }
    if(anyNA(x_nms)){
      warning ("`x` has `NA` names. Scaling possible but not recommended.",
               call. = FALSE)
    }else{
      cat("`x` has no `NA` names.", "\n")
    }
  }
  cat("Scaling is possible.", "\n")
}
