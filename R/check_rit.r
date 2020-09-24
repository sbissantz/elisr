
# Check lower bound -------------------------------------------------------

# Hint: Since stop() holds execution, no need for if...else... Just make sure
# the order is okay and that the call to the fun is after sclvals are checked

check_rit <- function(rit){
  if(is.null(rit))
    stop("No `rit_min` found. Specify a value between `0` and `1`",
         call. = FALSE)
  if (1 < rit || rit < 0)
    stop("`rit_min` must be a positive correlation between `0` and `1`.",
         call. = FALSE)
  rit_len <- length(rit)
  if (rit_len != 1)
    stop("`rit_min` must be a single value between `0` and `1`.",
         call. = FALSE)
}
