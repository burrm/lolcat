spc.controlviolation.zone2se <- function(x
                                         ,zone2se.point.count = 2
                                         ,zone2se.out.of = 3
                                         ,center.line
                                         ,standard.error
                                         ,zone2se.upper.bound = center.line + 2*standard.error
                                         ,zone2se.lower.bound = center.line - 2*standard.error
                                         ,...) {
  warning("Deprecated - will be removed in future release.")

  spc.controlviolation.zone1se(x = x 
                               ,zone1se.point.count = zone2se.point.count
                               ,zone1se.out.of = zone2se.out.of
                               ,zone1se.upper.bound = zone2se.upper.bound
                               ,zone1se.lower.bound = zone2se.lower.bound
                               )
}