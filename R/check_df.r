
# Check Data Frame --------------------------------------------------------
# Integrate a possibility to return df only in suits

check_df <- function(df, suits = FALSE){
  if(!is.data.frame(df))
    stop("`df` is not a data.frame.", call. = FALSE)
  df_len <- length(df)
  if(df_len < 2){
    stop("`df` has less than 2 variables.")
    }
  df_nms <- names(df)
  df_unms <- unique(df_nms)
  if(is.null(names)){
    stop("`df` has no (col)names.", call. = FALSE)
  }else{
    if(!identical(df_nms, df_unms)) {
    warning ("`df` names are not unique.", call. = FALSE)
    }
    if(anyNA(df_nms)){
      warning ("`df` has `NA` names.", call. = FALSE)
    }
  }
  if(suits)
    return(df)
}
