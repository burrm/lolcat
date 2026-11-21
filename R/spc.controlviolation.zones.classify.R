#' Classify SPC Chart Series By Zone
#' 
#' Identify which zone points in a chart series are part of.
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
#' @return A character vector with same length as chart.series identifying which zone a point is part of. Possible values are:
#' UU - beyond zone A/UCL
#' UA - Zone A above center line
#' UB - Zone B above center line
#' UC - Zone C above center line
#' CC - On Center Line
#' LC - Zone C below center line
#' LB - Zone B below center line
#' LA - Zone A below center line
#' LL - beyond zone A/LCL
spc.controlviolation.zones.classify <- function(
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
    ret <- sapply(1:length(chart.series), FUN = function(i) {
        if (chart.series[i] > center.line[i]) {
            if (chart.series[i] > zone.a.upper[i]) {
                "UU"
            } else if (chart.series[i] <= zone.a.upper[i] & chart.series[i] > zone.ab.upper[i]) {
                "UA"
            } else if (chart.series[i] <= zone.ab.upper[i] & chart.series[i] > zone.bc.upper[i]) {
                "UB"
            } else if (chart.series[i] <= zone.bc.upper[i]) {
                "UC"
            }
        } else if (chart.series[i] < center.line[i]) {
            if (chart.series[i] < zone.a.lower[i]) {
                "LL"
            } else if (chart.series[i] >= zone.a.lower[i] & chart.series[i] < zone.ab.lower[i]) {
                "LA"
            } else if (chart.series[i] >= zone.ab.lower[i] & chart.series[i] < zone.bc.lower[i]) {
                "LB"
            } else if (chart.series[i] >= zone.bc.lower[i]) {
                "LC"
            }
        } else {
            "CC"
        }
    })

    ret    
}