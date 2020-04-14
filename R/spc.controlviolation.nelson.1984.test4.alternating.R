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
 alt<-rep(F,length(signdiffs))

 alt.count <- 1


 for (i in 2:length(signdiffs)) {
  if (signdiffs[i-1] != 0 & signdiffs[i-1] == -1*signdiffs[i]) {
    alt.count <- alt.count +1
    
    if (alt.count >= (point.count-1)) {
      alt[(i-(point.count-1)):i] <- T
    }
  } else {
    alt.count <- 1
  }
 }

 alt<-c(alt[1],alt)


 alt[which(is.na(alt))] <- F

 alt

}