#Naming convention - variables charts
#          var/attr  Loc CL      Dispersion Input
#spc.chart.variables.mean   .and.meanstandarddeviation      .simple ()

spc.chart.variables.mean.and.meanstandarddeviation.simple <- function(
    means
    ,standard.deviations
    ,sample.size
    ,x = 1:length(means)

    ,chart1.main     = "Mean Chart (Mean Center Line)"
    ,chart1.ylab     = "Mean"
    ,chart1.center.line = rep(mean(means), length(means))
    ,chart1.control.limits.ucl = chart1.center.line + spc.constant.calculation.A3(sample.size, n.sigma = 3)*mean(standard.deviations)
    ,chart1.zone.a.upper  = chart1.control.limits.ucl
    ,chart1.zone.ab.upper = chart1.center.line + spc.constant.calculation.A3(sample.size, n.sigma = 2)*mean(standard.deviations)
    ,chart1.zone.bc.upper = chart1.center.line + spc.constant.calculation.A3(sample.size, n.sigma = 1)*mean(standard.deviations)


    ,chart1.control.limits.lcl = chart1.center.line - spc.constant.calculation.A3(sample.size, n.sigma = 3)*mean(standard.deviations)
    ,chart1.zone.a.lower  = chart1.control.limits.lcl
    ,chart1.zone.ab.lower = chart1.center.line - spc.constant.calculation.A3(sample.size, n.sigma = 2)*mean(standard.deviations)
    ,chart1.zone.bc.lower = chart1.center.line - spc.constant.calculation.A3(sample.size, n.sigma = 1)*mean(standard.deviations)

    ,chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4()

    ,chart2.main      = "Standard Deviation Chart (Mean Center Line)"
    ,chart2.ylab      = "Standard Deviation"
    ,chart2.center.line = rep(mean(standard.deviations), length(standard.deviations))
    ,chart2.control.limits.ucl = chart2.center.line*spc.constant.calculation.B4(sample.size, n.sigma = 3)
    ,chart2.zone.a.upper  = chart2.control.limits.ucl
    ,chart2.zone.ab.upper = chart2.center.line*spc.constant.calculation.B4(sample.size, n.sigma = 2)
    ,chart2.zone.bc.upper = chart2.center.line*spc.constant.calculation.B4(sample.size, n.sigma = 1)

    ,chart2.control.limits.lcl = chart2.center.line*spc.constant.calculation.B3(sample.size, n.sigma = 3)
    ,chart2.zone.a.lower  = chart2.control.limits.lcl
    ,chart2.zone.ab.lower = chart2.center.line*spc.constant.calculation.B3(sample.size, n.sigma = 2)
    ,chart2.zone.bc.lower = chart2.center.line*spc.constant.calculation.B3(sample.size, n.sigma = 1)

    ,chart2.control.rules = spc.rulesets.outside.limits()

    ,...
) {

#argg <- c(as.list(environment()), list(...))
#    print(argg)

ret <- list(
  description = "Mean and Standard Deviation Chart Information",
  x.labels = x #label series
  
  ,chart1.series = means   #data series
  ,chart1.main     = chart1.main
  ,chart1.ylab     = chart1.ylab
  ,chart1.center.line = chart1.center.line
  ,chart1.control.limits.ucl = chart1.control.limits.ucl
  ,chart1.zone.a.upper = chart1.zone.a.upper
  ,chart1.zone.ab.upper = chart1.zone.ab.upper
  ,chart1.zone.bc.upper = chart1.zone.bc.upper
  ,chart1.control.limits.lcl = chart1.control.limits.lcl
  ,chart1.zone.a.lower = chart1.zone.a.lower
  ,chart1.zone.ab.lower = chart1.zone.ab.lower
  ,chart1.zone.bc.lower = chart1.zone.bc.lower
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

  ,chart2.series     = standard.deviations
  ,chart2.main       = chart2.main
  ,chart2.ylab       = chart2.ylab
  ,chart2.center.line = chart2.center.line
  ,chart2.control.limits.ucl = chart2.control.limits.ucl
  ,chart2.zone.a.upper = chart2.zone.a.upper
  ,chart2.zone.ab.upper = chart2.zone.ab.upper
  ,chart2.zone.bc.upper = chart2.zone.bc.upper
  ,chart2.control.limits.lcl = chart2.control.limits.lcl
  ,chart2.zone.a.lower = chart2.zone.a.lower
  ,chart2.zone.ab.lower = chart2.zone.ab.lower
  ,chart2.zone.bc.lower = chart2.zone.bc.lower
  ,chart2.is.control.violation = spc.controlviolation.evaluate.rules(
    control.rules      = chart2.control.rules,
    chart.series       = standard.deviations, 
    center.line        = chart2.center.line,
    control.limits.ucl = chart2.control.limits.ucl,
    zone.a.upper       = chart2.zone.a.upper,
    zone.ab.upper      = chart2.zone.ab.upper,
    zone.bc.upper      = chart2.zone.bc.upper,
    control.limits.lcl = chart2.control.limits.lcl,
    zone.a.lower       = chart2.zone.a.lower,
    zone.ab.lower      = chart2.zone.ab.lower,
    zone.bc.lower      = chart2.zone.bc.lower,
   )
)

spc.chart.simple(
  x = x #label series
  
  ,chart1.series = means   #data series
  ,chart1.main     = chart1.main
  ,chart1.ylab     = chart1.ylab
  ,chart1.center.line = chart1.center.line
  ,chart1.control.limits.ucl = chart1.control.limits.ucl
  ,chart1.control.limits.lcl = chart1.control.limits.lcl
  ,chart1.is.control.violation = ret$chart1.is.control.violation$overall.results

  ,chart2.series     = standard.deviations
  ,chart2.main       = chart2.main
  ,chart2.ylab       = chart2.ylab
  ,chart2.center.line = chart2.center.line
  ,chart2.control.limits.ucl = chart2.control.limits.ucl 
  ,chart2.control.limits.lcl = chart2.control.limits.lcl 
  ,chart2.is.control.violation = ret$chart2.is.control.violation$overall.results

)
    
  invisible(ret)

}
