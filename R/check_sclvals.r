
# Check Scaling Values ----------------------------------------------------
# TODO Name sclvals to scl_ends, scl_ep's, sclrng

# Hint: Since stop() holds execution, no need for if...else... Just make sure
# the order is okay and that the call to the fun is after sclvals are checked

check_sclvals <- function(sclvals){
  if(is.null(sclvals))
    stop("No `sclvals` found. Specify a vector of the form `c(min,max)`",
         call. = FALSE)
  if (!is.vector(sclvals))
    stop("`sclvals` must be a vector of the form `c(min,max)`.",
         call. = FALSE)
  sclvals_len <- length(sclvals)
  if (isFALSE(sclvals_len == 2))
    stop("'sclvals' takes 2 values a `min` and a `max`, e.g., `c(min, max)`.",
           call. = FALSE)
  if(sclvals[1] > sclvals[2])
    stop("`sclvals` min is is greater than its max. Consider `c(min, max)`.",
           call. = FALSE)
}
