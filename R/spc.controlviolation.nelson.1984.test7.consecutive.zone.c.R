spc.controlviolation.nelson.1984.test7.consecutive.zone.c <- function(
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

    point.count = 15,

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

    criteria.upper <- c("UC")
    criteria.center <- c("CC")
    criteria.lower <- c("LC")

    criteria.combined <- c(criteria.upper, criteria.center, criteria.lower)

    if (length(zones.out) >= point.count) {
        if (length(zones.out) >= point.count) {
            subtract.len <- point.count - 1

            for (i in point.count:length(zones.out)) {
                if (all(zones.out[(i-subtract.len):i] %in% criteria.combined) & any(zones.out[(i-subtract.len):i] %in% criteria.upper) & any(zones.out[(i-subtract.len):i] %in% criteria.lower)) {
                     ret[(i-subtract.len):i] <- T   
                } 
            }
        }
    }

    ret[which(is.na(ret))] <- F

    ret
}