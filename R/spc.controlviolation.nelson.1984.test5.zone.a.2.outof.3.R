spc.controlviolation.nelson.1984.test5.zone.a.2.outof.3 <- function(
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

    point.count = 2,
    outof = 3,

    ...
) {
  zones.out <- spc.controlviolation.zones.classify(
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

    ret <- rep(F, length(chart.series))

    criteria.upper <- c("UU","UA")
    criteria.lower <- c("LL","LA")

    if (length(zones.out) >= point.count) {
        if (all(zones.out[1:point.count] %in% criteria.upper) |
            all(zones.out[1:point.count] %in% criteria.lower)
        ) {
            ret[1:point.count] <- T
        }

        if (length(zones.out) >= outof) {
            subtract.len <- outof - 1
            for (i in outof:length(zones.out)) {
                if (zones.out[i] %in% criteria.upper & sum(ifelse(zones.out[(i-subtract.len):i] %in% criteria.upper, 1, 0 )) >= point.count) {
                     ret[(i-subtract.len):i] <- T   
                } else if (zones.out[i] %in% criteria.lower & sum(ifelse(zones.out[(i-subtract.len):i] %in% criteria.lower,1,0)) >= point.count) {
                     ret[(i-subtract.len):i] <- T   
                }
            }
        }
    }

    ret[which(is.na(ret))] <- F

    ret
}