#' Control Violation Rule Template
#' 
#' Use this template to create your own chart rules for use in custom rule sets.
#'
#' @param chart.series Vector - Chart series
#' @param center.line Vector - Chart center line
#' @param control.limits.ucl Vector - Chart upper control limit (UCL)
#' @param zone.a.upper Vector - Chart Zone A boundary above center line.
#' @param zone.ab.upper Vector - Chart boundary between Zone A and Zone B above center line.
#' @param zone.bc.upper Vector - Chart boundary between Zone B and Zone C above center line.
#' @param control.limits.lcl Vector - Chart lower control limit (LCL)
#' @param zone.a.lower Vector - Chart Zone A boundary below center line.
#' @param zone.ab.lower Vector - Chart boundary between Zone A and Zone B below center line.
#' @param zone.bc.lower Vector - Chart boundary between Zone B and Zone C below center line.
#'
#' @return A logical vector with same length as chart.series identifying whether each point has a control violation or not. 
spc.controlviolation.template <- function(
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

    ...
) {
    ret <- rep(F, length(chart.series))

    ret    
}