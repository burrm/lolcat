spc.controlviolation.nelson.1984.test2.runs.below.create <- function(
    point.count = 9
) {
    p <- point.count

    fn <- function(
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

        point.count = p,

        ...
    ) {
      spc.controlviolation.nelson.1984.test2.runs.below(
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
    }

    fn
}