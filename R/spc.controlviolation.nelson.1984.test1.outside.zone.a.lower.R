spc.controlviolation.nelson.1984.test1.outside.zone.a.lower <- function(
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
    ret <- (chart.series < center.line & chart.series < zone.a.lower)

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