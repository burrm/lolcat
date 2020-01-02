spc.controlviolation.nelson.1984.test3.trends <- function(
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

    point.count = 6,
    strictly.increasing = T,
    strictly.decreasing = strictly.increasing,

    ...
) {
  ret.1 <- spc.controlviolation.nelson.1984.test3.trends.increasing(
      point.count = point.count,
      strictly.increasing = strictly.increasing,
      
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

  ret.2 <- spc.controlviolation.nelson.1984.test3.trends.decreasing(
      point.count = point.count,
      strictly.decreasing = strictly.decreasing,

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
  
  ret <- ret.1 | ret.2 
  ret[which(is.na(ret))] <- F
  ret

}