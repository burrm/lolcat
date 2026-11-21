#' Control Chart Rule Set - Nelson Rules 1,2,3,4  
#' 
#' Provides an SPC chart rule set using the first four Nelson rules: Points outside of limits, runs, trends, and alternating points. 
#'
#' @return A list with control violation evaluation functions to be used by spc.chart functions. 
spc.rulesets.nelson.1984.test.1.2.3.4 <- function() {
    list(
      outside.limits = spc.controlviolation.nelson.1984.test1.outside.zone.a,
      runs = spc.controlviolation.nelson.1984.test2.runs,
      trends = spc.controlviolation.nelson.1984.test3.trends,
      alternating = spc.controlviolation.nelson.1984.test4.alternating
    )
}