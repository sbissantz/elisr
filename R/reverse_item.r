
# Reverse Variables -------------------------------------------------------

# rvrs_var() takes the specified scale values (min & max), and a variable to
# reverse -- i.e. which is negatively correlating with any other item.

rvrs_var <- function(var, sclvals) {
  if(isFALSE(length(sclvals) == 2))
    stop("'sclvals' only takes 2 values (min & max)", call. = FALSE)
  if(sclvals[1] > sclvals[2])
    stop("'scalevals' min shouldn't be greater than its max",
         call. = FALSE)
    # ... -3 -2 -1 0 1 2 3 ...
    if(sclvals[1] < 0) {
      var_rev <- var * -1
    }
    # 0 1 2 3 4 5 6 ...
    if(sclvals[1] == 0 && sclvals[2] > 0){
      var_rev <- sclvals[2] - var
    }
    # 1 2 3 4 5 6 7
    if(sclvals[1] == 1 && sclvals[2] > 0){
      var_rev <- (sclvals[2] + 1) - var
    }
  return(var_rev)
}




