spc.controlviolation.excessive.moving.range <- function(x, standard.error, excessive.moving.range = 3.6864) {
  mr <- c(NA,diff(x))
  
  ret <- which(abs(mr) > standard.error * excessive.moving.range)
  
  ret[which(is.na(ret))] <- F
  
  ret
}