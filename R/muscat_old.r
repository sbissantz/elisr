rm(list=ls())
# Muscat gives back a scale universe sorted
# according to the principles of crystallization
muscat <- function(
  df, rit.min=.3, overlap=FALSE,
  negative.too=FALSE, scl.max=NULL,
  checkup=TRUE, overlap.with=NULL, uni.vrs=NULL,...)
  {
  # Allow checks to be turned off
  # Use isTRUE instead of ==TRUE
  if(isTRUE(checkup)){
    # ...stop if it is neither a Mat nor a df
    stopifnot(is.data.frame(df) || is.matrix(df))
    # ...We need colnames to (1) grep existing items from the scale
    # when we want to do an overlap. And (2) to 're-allocate' the
    # items # after scales are build. As data.frame coreces Mat and
    # assign colnames automatically.
    if(is.matrix(df)){
      df <- as.data.frame(df)}
    if(isFALSE(is.data.frame(df))){
      df <- as.data.frame(df)
    # ...If smo hands a df without colnames, assign colnames
    if(is.null( attr(df,"names"))){
    df.len <- length(df)
    # Nice trick from Norman Mattloff to use recycling for naming
    attr(df,"names") <- paste("V",seq(df.len), sep="")
    }
      }
    # If smo want to include negative items but misses to specify
    # a scl.max...
    if(isTRUE(negative.too) && is.null(scl.max))
      stop("Include negative Cor's? Set scl.max!",call. = FALSE)
    }else{
      # Always warn smo when someone set checkup off...
      warning("Feel the Force: chekup == FALSE!
              From great power comes great responsibility!"
              ,call. = FALSE)
      }
  # scl.bang creates a disjoint(1) uni.vrs
  # -- according to the principle of# cristallization
  scl.bang <- function(rit.min,df,negative.too,scl.max,...){
    uni.vrs <- list()
    while(ncol(df)>=2){
      uni.len <- length(uni.vrs)
      # Someday: include (Cor <- cor(df,...))
      # Smo can specify the NA options -- "use"
      Cor <- cor(df,...)
      maxcor <- max(Cor[Cor<1])
      if(maxcor < rit.min) break
      # Take the first(!) maximum
      Fmaxp <- which(Cor == maxcor,arr.ind = TRUE)[1,]
      uni.vrs[[uni.len + 1]] <- df[Fmaxp]
      df <- df[-Fmaxp]
      if( isFALSE(negative.too) ){
        while(ncol(df) >= 1){
          uni.len <- length(uni.vrs)
          Cor <- cor(rowSums(uni.vrs[[uni.len]]),df)
          maxcor <- max(Cor[Cor<1])
          if(maxcor < rit.min) break
          Fmaxp <- which(Cor == maxcor)
          uni.vrs[[uni.len]] <- cbind(uni.vrs[[uni.len]], df[Fmaxp])
          df <- df[-Fmaxp]
          }
        }else{
          while(ncol(df)>=1){
            uni.len <- length(uni.vrs)
            Cor <- cor(rowSums(uni.vrs[[uni.len]]),df)
            maxcor <- max(abs(Cor[Cor<1]))
            if(maxcor < rit.min) break
            Fmaxp <- which(abs(Cor) == maxcor)
            corsign <- sign(Cor[Fmaxp])
            if(corsign >= 0){
              uni.vrs[[uni.len]] <- cbind(uni.vrs[[uni.len]], df[Fmaxp])
              }else{
                var.rev <- (scl.max + 1) - df[Fmaxp]
                uni.vrs[[uni.len]] <- cbind(uni.vrs[[uni.len]], var.rev)
                }
            df <- df[-Fmaxp]
          }
        }
      }
    return(uni.vrs)
    }
  # Meltdown does overlapping scale building using a pre-build
  # wrk.vrs which includes the variables to work with and
  # and ovl.vrs which hoovers the information from the scale
  meltdown <- function(ovl.vrs, wrk.vrs){
    while(ncol(wrk.vrs)>=1){
    # Someday: include (Cor <- cor(df,...))
    # TODO: Let user choose NA.option
      Cor <- cor(rowSums(ovl.vrs),wrk.vrs,...)
      maxcor <- max(Cor[Cor<1])
      if(maxcor < rit.min) break
      fstmaxp <- which(Cor == maxcor)
      ovl.vrs <- cbind(ovl.vrs, wrk.vrs[fstmaxp])
      wrk.vrs <- wrk.vrs[-fstmaxp]
      }
    return(ovl.vrs)
  }
# nuc.nmr extract the nms of the build nucleus'
# from a build uni.vrs
  nuc.nmr <- function(scl.nms){
    scl.nms[c(1,2)]
  }
# Vrsr pre-builds a ls of overlap df's (ovl.vrs') and wprking df's
# for the overlapping procedure.
  vrsr <- function(vrs,nms){
    # ovl.vrsr extracts a sub.df, by matching the input names
    # against # the names specified df.
    ovl.vrsr <- function(nms){
      df[ , nms]
      }
    # ovl.vrsr extracts a sub.df, by excluding the namesfrom the
    # specified df
    wrk.vrsr <- function(nms){
      df[ , -which(colnames(df) %in% nms)]
    }
    # Gives me a ls of overlaping verses
    if(vrs == "ovl"){
      return(lapply(nms, ovl.vrsr))
      }
    # Gives me a ls of working verses
    if(vrs == "wrk"){
      return(lapply(nms, wrk.vrsr))
      }
  }
  # If smo wants only a disjoint scale building procedure
  if(isFALSE(overlap)){
    scl.bang(rit.min,df,negative.too,scl.max)
    }else{
      # If smo built a disjoint scale, he can use that
      # uni.vrs and only gets the overlap on top. If not..
      # We need to build it. Hint: Improves exeution time
      if(is.null(uni.vrs)){
      uni.vrs <- scl.bang(rit.min,df,negative.too,scl.max)
      }
      scl.nms <- lapply(uni.vrs,colnames)
      nuc.nms <- lapply(scl.nms, nuc.nmr)
      # Overlap using the full scales build by scl.bang
      if(overlap.with == "full.scale"){
        ovl.vrs <- vrsr("ovl",scl.nms)
        wrk.vrs <- vrsr("wrk",scl.nms)
        }
      # Overlap using only the nucleus' build by  scl.bang
      if(overlap.with == "nucleus"){
        ovl.vrs <- vrsr("ovl",nuc.nms)
        wrk.vrs <- vrsr("wrk",nuc.nms)
        }
      return(mapply(meltdown, ovl.vrs, wrk.vrs,
                    SIMPLIFY = FALSE))
      }
  }

# Bottle neck?
system.time(muscat(df=mtcars))
uni.vrs <- muscat(df=mtcars)

system.time(muscat(df=mtcars,overlap = TRUE, overlap.with = "nucleus",uni.vrs=uni.vrs))


(uni.vrs <- muscat(df=mtcars,
       rit.min=.6,
       negative.too = FALSE))

muscat(df=mtcars,
       rit.min=.3,
       negative.too = TRUE,
       scl.max = 7)

muscat(df=mtcars,
       rit.min=.6,
       negative.too = TRUE,
       scl.max = 7,
       checkup = FALSE)

muscat(df=mtcars,
       rit.min=.6,
       negative.too = TRUE)

muscat(df=df,
       rit.min=0.3,
       negative.too = TRUE,
       scl.max = 7)


























































