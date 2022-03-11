#' Control Violation Rule Evaluation
#' 
#' This function is used to evaluate control rules in the SPC charting functions.
#'
#' @param control.rules List - list of functions following conventions in spc.controlviolation.template
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
#' @return A data structure with individual rule violations (sharing names with control.rules) 
#' and whether each point is subject to one or more control violations. 
spc.controlviolation.evaluate.rules <- function(
    control.rules = list(),

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
    out <- list()

    if (length(control.rules) > 0) {
        #correct lengths
        if (length(chart.series) != length(center.line)) {
            center.line <- rep_len(center.line, length(chart.series))
        }
   
        if (length(chart.series) != length(control.limits.ucl)) {
            control.limits.ucl <- rep_len(control.limits.ucl, length(chart.series))
        }

        if (length(chart.series) != length(zone.a.upper)) {
            zone.a.upper <- rep_len(zone.a.upper, length(chart.series))
        }

        if (length(chart.series) != length(zone.ab.upper)) {
            zone.ab.upper <- rep_len(zone.ab.upper, length(chart.series))
        }

        if (length(chart.series) != length(zone.bc.upper)) {
            zone.bc.upper <- rep_len(zone.bc.upper, length(chart.series))
        }

        if (length(chart.series) != length(control.limits.lcl)) {
            control.limits.lcl <- rep_len(control.limits.lcl, length(chart.series))
        }

        if (length(chart.series) != length(zone.a.lower)) {
            zone.a.lower <- rep_len(zone.a.lower, length(chart.series))
        }

        if (length(chart.series) != length(zone.ab.lower)) {
            zone.ab.lower <- rep_len(zone.ab.lower, length(chart.series))
        }

        if (length(chart.series) != length(zone.bc.lower)) {
            zone.bc.lower <- rep_len(zone.bc.lower, length(chart.series))
        }


        out <- lapply(control.rules, FUN = function(fn) {
            fn(
                chart.series = chart.series, 

                center.line = center.line,

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
        })

        #str(out)
        #print(out)

        for (i in 1:length(out)) {
            ret <- ret | out[[i]]
        }
    }

    list(rule.results = out, overall.results = ret)
}