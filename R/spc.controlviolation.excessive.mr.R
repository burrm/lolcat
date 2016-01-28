spc.controlviolation.excessive.mr <- function(x, standard.error, excessive.mr = 3.6864) {
  mr <- c(NA,diff(x))
  
  ret <- which(abs(mr) > standard.error * excessive.mr)
  
  ret[which(is.na(ret))] <- F
  
  ret
}