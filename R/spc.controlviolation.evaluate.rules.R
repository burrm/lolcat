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

    ret
}