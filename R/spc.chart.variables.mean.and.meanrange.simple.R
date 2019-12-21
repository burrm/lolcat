#Naming convention - variables charts
#          var/attr  Loc CL      Dispersion Input
#spc.chart.variables.mean   .and.range      .simple ()

spc.chart.variables.mean.and.meanrange.simple <- function(
    means
    ,ranges
    ,sample.size
    ,x = 1:length(means)

    ,chart1.main     = "Mean Chart"
    ,chart1.ylab     = "Mean"
    ,chart1.center.line = mean(means)
    ,chart1.control.limits.ucl = chart1.center.line + spc.constant.calculation.A2(sample.size, n.sigma = 3)*mean(ranges)
    ,chart1.zone.a.upper  = chart1.control.limits.ucl
    ,chart1.zone.ab.upper = chart1.center.line + spc.constant.calculation.A2(sample.size, n.sigma = 2)*mean(ranges)
    ,chart1.zone.bc.upper = chart1.center.line + spc.constant.calculation.A2(sample.size, n.sigma = 1)*mean(ranges)


    ,chart1.control.limits.lcl = chart1.center.line - spc.constant.calculation.A2(sample.size, n.sigma = 3)*mean(ranges)
    ,chart1.zone.a.lower  = chart1.control.limits.lcl
    ,chart1.zone.ab.lower = chart1.center.line - spc.constant.calculation.A2(sample.size, n.sigma = 2)*mean(ranges)
    ,chart1.zone.bc.lower = chart1.center.line - spc.constant.calculation.A2(sample.size, n.sigma = 1)*mean(ranges)

    ,chart1.control.rules = list(
      outside.limits = spc.controlviolation.nelson.1984.test1.outside.zone.a,
      runs = spc.controlviolation.nelson.1984.test2.runs,
      trends = spc.controlviolation.nelson.1984.test3.trends,
      alternating = spc.controlviolation.nelson.1984.test4.alternating
    )

    ,chart2.main      = "Range Chart"
    ,chart2.ylab      = "Range"
    ,chart2.center.line = mean(ranges)
    ,chart2.control.limits.ucl = chart2.center.line*spc.constant.calculation.D4(sample.size)
    ,chart2.zone.a.upper  = chart2.control.limits.ucl
    ,chart2.zone.ab.upper = NA
    ,chart2.zone.bc.upper = NA

    ,chart2.control.limits.lcl = chart2.center.line*spc.constant.calculation.D3(sample.size)
    ,chart2.zone.a.lower  = chart2.control.limits.lcl
    ,chart2.zone.ab.lower = NA
    ,chart2.zone.bc.lower = NA

    ,chart2.control.rules = list(
      outside.limits = spc.controlviolation.nelson.1984.test1.outside.zone.a
    )

    ,...
) {

#argg <- c(as.list(environment()), list(...))
#    print(argg)

spc.chart.simple(
  x = x #label series
  
  ,chart1.series = means   #data series
  ,chart1.main     = chart1.main
  ,chart1.ylab     = chart1.ylab
  ,chart1.center.line = chart1.center.line
  ,chart1.control.limits.ucl = chart1.control.limits.ucl
  ,chart1.control.limits.lcl = chart1.control.limits.lcl
  ,chart1.is.control.violation = spc.controlviolation.evaluate.rules(
    control.rules      = chart1.control.rules,
    chart.series       = means, 
    center.line        = chart1.center.line,
    control.limits.ucl = chart1.control.limits.ucl,
    zone.a.upper       = chart1.zone.a.upper,
    zone.ab.upper      = chart1.zone.ab.upper,
    zone.bc.upper      = chart1.zone.bc.upper,
    control.limits.lcl = chart1.control.limits.lcl,
    zone.a.lower       = chart1.zone.a.lower,
    zone.ab.lower      = chart1.zone.ab.lower,
    zone.bc.lower      = chart1.zone.bc.lower,
    ...
  )

  ,chart2.series     = ranges
  ,chart2.main       = chart2.main
  ,chart2.ylab       = chart2.ylab
  ,chart2.center.line = chart2.center.line
  ,chart2.control.limits.ucl = chart2.control.limits.ucl 
  ,chart2.control.limits.lcl = chart2.control.limits.lcl 
  ,chart2.is.control.violation = spc.controlviolation.evaluate.rules(
    control.rules      = chart2.control.rules,
    chart.series       = ranges, 
    center.line        = chart2.center.line,
    control.limits.ucl = chart2.control.limits.ucl,
    zone.a.upper       = chart2.zone.a.upper,
    zone.ab.upper      = chart2.zone.ab.upper,
    zone.bc.upper      = chart2.zone.bc.upper,
    control.limits.lcl = chart2.control.limits.lcl,
    zone.a.lower       = chart2.zone.a.lower,
    zone.ab.lower      = chart2.zone.ab.lower,
    zone.bc.lower      = chart2.zone.bc.lower,
    ...
  )

)
    
}
