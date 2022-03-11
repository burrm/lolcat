#' Control Violations - Nelson Rule 2 - Runs Above or Below Center Line 
#'
#' Runs are sequential points above or below the center line on a control chart.
#' Too many sequential points above or below the center line indicates that the process
#' may be behaving non-randomly and has one or more special causes of variation present.  
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
#' @param point.count Scalar - Number of points to use for detecting violation
#' @param ... Additional parameters
#'
#' @return Vector of logical values corresponding to whether points in chart.series violate this rule. 
spc.controlviolation.nelson.1984.test2.runs <- function(
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

    point.count = 9,

    ...
) {
  ret.1 <- spc.controlviolation.nelson.1984.test2.runs.above(
      point.count = point.count,
      
      center.line = center.line,

      chart.series = chart.series, 
      control.limits.ucl = control.limits.ucl,
      zone.a.upper = zone.a.upper,
      zone.ab.upper = zone.ab.upper,
      zone.bc.upper = zone.bc.upper,

      control.limits.lcl = control.limits.lcl,
      zone.a.lower = zone.a.lower,
      zone.ab.lower = zone.ab.lower,
      zone.bc.lower = zone.bc.lower,

      ... 
      )

  ret.2 <- spc.controlviolation.nelson.1984.test2.runs.below(
      point.count = point.count,
      
      center.line = center.line,

      chart.series = chart.series, 
      control.limits.ucl = control.limits.ucl,
      zone.a.upper = zone.a.upper,
      zone.ab.upper = zone.ab.upper,
      zone.bc.upper = zone.bc.upper,

      control.limits.lcl = control.limits.lcl,
      zone.a.lower = zone.a.lower,
      zone.ab.lower = zone.ab.lower,
      zone.bc.lower = zone.bc.lower,

      ... 
      )
  
  ret <-ret.1 | ret.2
  ret[which(is.na(ret))] <- F

  ret 
}