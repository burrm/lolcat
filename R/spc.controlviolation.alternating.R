spc.controlviolation.alternating <- function(x, alternating.value.count = 14, ...) {
  signdiffs<-sign(diff(x))
  
  alt<-logical(length(signdiffs))
  
  for (i in 2:length(signdiffs)) {
    alt[i] <- signdiffs[i-1] != 0 & signdiffs[i-1] == -1*signdiffs[i]
  }
  
  alt[1] <- alt[2]
  alt<-c(alt[1],alt)
  
  observed.run.lengths<-rle(alt)
  
  ret<-logical(0)
  
  for (i in 1:length(observed.run.lengths$lengths)) {
    is.run.violation <- observed.run.lengths$lengths[i] >= alternating.value.count & observed.run.lengths$values[i]
    ret <- c(ret, rep(is.run.violation, observed.run.lengths$lengths[i]))
  }
  
  ret
}