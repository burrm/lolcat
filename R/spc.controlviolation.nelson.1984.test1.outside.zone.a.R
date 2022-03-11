#' Control Violations - Nelson Rule 1 - Points Outside Zone A 
#' 
#' Zone A (above and below the center line) typically corresponds with the upper and 
#' lower control limits for the chart.
#' This rule identifies points outside of Zone A limits. 
#' Points in chart.series that are outside of the control limits may indicate that 
#' a special cause of variability is present that is causing the process to exhibit higher variability 
#' than would be expected for a process exhibiting a state of control. 
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
#' @param ... Additional parameters
#'
#' @return Vector of logical values corresponding to whether points in chart.series violate this rule. 
spc.controlviolation.nelson.1984.test1.outside.zone.a <- function(
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
    ret <- (chart.series > center.line & chart.series > zone.a.upper) | (chart.series < center.line & chart.series < zone.a.lower)

#    ret <- sapply(chart.series, FUN = function(i) {
#        pt <- F
#
#        if ((i > center.line & i > zone.a.upper) | (i < center.line & i < zone.a.lower)) {
#            pt <- T
#        }
#
#        if (is.na(pt)) {
#            pt <- F
#        }
#
#        pt
#    })

    ret[which(is.na(ret))] <- F
    ret 
}