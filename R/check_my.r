# TODO use hasName?
# shouldnt be NA
# should be non-Null
# should be unique
# TODO: FUN has name is from utils
#stopifnot(all(hasName(df, names(df))))


  #if (is.null(sclvals))
  #  stop("No 'sclvals' found. Please specify one.", call. = FALSE)
  # if (sclvals <= 0)
  #  stop("disunito() can only deal with 'values > 0'. Please recode the scale",
  #       call. = FALSE)

check_my <- function(x){
  if(!is.data.frame(x))
    stop("`x` is not a data.frame.", call. = FALSE)
  x_len <- length(x)
  if(x_len < 2){
    stop("`x` has less than 2 variables.")
    }
  x_nms <- names(x)
  x_unms <- unique(x_nms)
  if(is.null(names)){
    stop("`x` has no (col)names.", call. = FALSE)
  }else{
    if(!identical(x_nms, x_unms)) {
    warning ("`x` names are not unique.", call. = FALSE)
    }
    if(anyNA(x_nms)){
      warning ("`x` has `NA` names.", call. = FALSE)
    }
  }
  x
}