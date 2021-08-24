#' Control Chart Rule Set - Nelson Rules 1-8  
#' 
#' Provides an SPC chart rule set using eight Nelson rules: Points outside of limits, runs, 
#' trends, and alternating points, 2/3 points in zone A, 4/5 points in zones A/B, 
#' consecutive points in zone C, and consecutive points in zones A/B. 
#'
#' @return A list with control violation evaluation functions to be used by spc.chart functions. 
spc.rulesets.nelson.1984.test.1.2.3.4.5.6.7.8 <- function() {
    list(
      outside.limits      = spc.controlviolation.nelson.1984.test1.outside.zone.a,
      runs                = spc.controlviolation.nelson.1984.test2.runs,
      trends              = spc.controlviolation.nelson.1984.test3.trends,
      alternating         = spc.controlviolation.nelson.1984.test4.alternating,
      zone.a              = spc.controlviolation.nelson.1984.test5.zone.a.2.outof.3,
      zone.ab             = spc.controlviolation.nelson.1984.test6.zone.ab.4.outof.5,
      consecutive.zone.c  = spc.controlviolation.nelson.1984.test7.consecutive.zone.c,
      consecutive.zone.ab = spc.controlviolation.nelson.1984.test8.consecutive.zone.ab
    )
}