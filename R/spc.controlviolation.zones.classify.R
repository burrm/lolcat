#
# UU - beyond zone A/UCL
# UA - Zone A above center line
# UB - Zone B above center line
# UC - Zone C above center line
# CC - On Center Line
# LC - Zone C below center line
# LB - Zone B below center line
# LA - Zone A below center line
# LL - beyond zone A/LCL
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
    ret <- sapply(chart.series, FUN = function(x) {
        if (x > center.line) {
            if (x > zone.a.upper) {
                "UU"
            } else if (x <= zone.a.upper & x > zone.ab.upper) {
                "UA"
            } else if (x <= zone.ab.upper & x > zone.bc.upper ) {
                "UB"
            } else if (x <= zone.bc.upper) {
                "UC"
            }
        } else if (x < center.line) {
            if (x < zone.a.lower) {
                "LL"
            } else if (x >= zone.a.lower & x < zone.ab.lower) {
                "LA"
            } else if (x >= zone.ab.lower & x < zone.bc.lower ) {
                "LB"
            } else if (x >= zone.bc.lower) {
                "LC"
            }
        } else {
            "CC"
        }
    })

    ret    
}