
hubble <- function(sub.vrs) {
# wrk.vrsr build subuniverses from a seq of numbers -- I use that
# to post-determine how the universe was build (sequentially)
  wrk.vrsr <- function(seq) {
    return(sub.vrs[seq])
  }

# alphawave calculates Cronbachs Alpha from a wrk.vrs. Since
# wrk.vrs is a sequentially build Ls of the dfs to the according
# step in that process, I use it to explore the influence of every
# new item added to the scl
  alphawve <- function(wrk.vrs){
    Cor <- cor(wrk.vrs)
    rbar <- mean(Cor[lower.tri(Cor)])
    m <- length(wrk.vrs)
    return((m * rbar) / (1 + rbar * (m - 1)))
  }
# ritr does the same as alphawave but with rit
  ritr <- function(wrk.vrs){
   wrk.len <- length(wrk.vrs)
    nucleus <- rowSums(wrk.vrs[-wrk.len])
    addtnl <- wrk.vrs[wrk.len]
    return(cor(nucleus,addtnl))
  }

  sv.len <- length(sub.vrs)
  seq.ls <- lapply(2:sv.len, seq)
  wrk.vrs <- lapply(seq.ls,wrk.vrsr)
  col.nms <- colnames(sub.vrs)
  col.len <- length(col.nms)
  var.nms <- c(paste(col.nms[seq(2)], collapse = ", "),
                 col.nms[seq(3,col.len)])

  alpha <- lapply(wrk.vrs, alphawve)
  rit <- lapply(wrk.vrs,ritr)

  return(cbind(var.nms, alpha,rit))
}

#sub.vrs <- uni.vrs[[1]]
#hubble(sub.vrs)
