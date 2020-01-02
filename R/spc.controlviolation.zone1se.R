spc.controlviolation.zone1se <- function(x
                                         ,zone1se.point.count = 4
                                         ,zone1se.out.of = 5
                                         ,center.line
                                         ,standard.error
                                         ,zone1se.upper.bound = center.line + standard.error
                                         ,zone1se.lower.bound = center.line - standard.error
                                         ,...) {
  out.of.limits <- as.integer(x > zone1se.upper.bound | x < zone1se.lower.bound)

  ret<-rep(F,length(x))
  
  for (i in (zone1se.out.of+1):length(x)) {
    if (sum(out.of.limits[(i-zone1se.out.of):i]) >= zone1se.point.count) {
      ret[(i-zone1se.out.of):i] <- TRUE
    }
  }
  
  ret
}