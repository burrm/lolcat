spc.controlviolation.nelson.1984.test4.alternating <- function(
    chart.series = NA,

    center.line = NA,

    control.limits.ucl = NA,
    zone.a.upper = NA,
    zone.ab.upper = NA,
    zone.bc.upper = NA,

    control.limits.lcl = NA,
    zone.a.lower = NA,
    zone.ab.lower = NA,
    zone.bc.lower = NA,

    point.count = 14,

    ...
) {
  signdiffs<-sign(diff(chart.series))
  
  alt<-logical(length(signdiffs))
  
  for (i in 2:length(signdiffs)) {
    alt[i] <- signdiffs[i-1] != 0 & signdiffs[i-1] == -1*signdiffs[i]
  }
  
  alt[1] <- alt[2]
  alt<-c(alt[1],alt)
  
  observed.run.lengths<-rle(alt)
  
  ret<-logical(0)
  
  for (i in 1:length(observed.run.lengths$lengths)) {
    is.run.violation <- observed.run.lengths$lengths[i] >= point.count & observed.run.lengths$values[i]
    ret <- c(ret, rep(is.run.violation, observed.run.lengths$lengths[i]))
  }
  
  ret[which(is.na(ret))] <- F

  ret    

}