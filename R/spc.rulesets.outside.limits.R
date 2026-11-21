#' Control Chart Rule Set - Outside Limits  
#' 
#' Provides an SPC chart rule set that reports control violations for points outside of control limits. 
#'
#' @return A list with control violation evaluation functions to be used by spc.chart functions. 
spc.rulesets.outside.limits <- function() {
    list(
      outside.limits = spc.controlviolation.nelson.1984.test1.outside.zone.a
    )
}