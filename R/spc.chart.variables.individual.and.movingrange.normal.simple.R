#Naming convention - variables charts
#          var/attr  Loc CL      Dispersion Input
#spc.chart.variables.individual.and.movingrange      .simple ()

spc.chart.variables.individual.and.movingrange.normal.simple <- function(
    individuals
    ,movingranges              = c(NA, abs(diff(individuals)))
    ,x                         = 1:length(individuals)

    ,sample.size               = rep(2, length(individuals))

    ,chart1.main               = "Individuals Chart\n(Normal Distribution)"
    ,chart1.ylab               = "Individual"
    ,chart1.center.line        = rep(mean(individuals), length(x))
    ,chart1.control.limits.ucl = chart1.center.line + 2.6587*3*chart2.center.line/3
    ,chart1.zone.a.upper       = chart1.control.limits.ucl
    ,chart1.zone.ab.upper      = chart1.center.line + 2.6587*2*chart2.center.line/3
    ,chart1.zone.bc.upper      = chart1.center.line + 2.6587*chart2.center.line/3


    ,chart1.control.limits.lcl = chart1.center.line - 2.6587*3*chart2.center.line/3
    ,chart1.zone.a.lower       = chart1.control.limits.lcl
    ,chart1.zone.ab.lower      = chart1.center.line - 2.6587*2*chart2.center.line/3
    ,chart1.zone.bc.lower      = chart1.center.line - 2.6587*chart2.center.line/3

    ,chart1.control.rules      = spc.rulesets.nelson.1984.test.1.2.3.4()

    ,chart2.main               = "Moving Range Chart"
    ,chart2.ylab               = "Moving Range"
    ,chart2.center.line        = rep(mean(na.omit(movingranges)), length(x))
    ,chart2.control.limits.ucl = chart2.center.line*spc.constant.calculation.D4(sample.size = sample.size, n.sigma = 3)
    ,chart2.zone.a.upper       = chart2.control.limits.ucl
    ,chart2.zone.ab.upper      = chart2.center.line*spc.constant.calculation.D4(sample.size = sample.size, n.sigma = 2)
    ,chart2.zone.bc.upper      = chart2.center.line*spc.constant.calculation.D4(sample.size = sample.size, n.sigma = 1)

    ,chart2.control.limits.lcl = chart2.center.line*spc.constant.calculation.D3(sample.size = sample.size, n.sigma = 3)
    ,chart2.zone.a.lower       = chart2.control.limits.lcl
    ,chart2.zone.ab.lower      = chart2.center.line*spc.constant.calculation.D3(sample.size = sample.size, n.sigma = 2)
    ,chart2.zone.bc.lower      = chart2.center.line*spc.constant.calculation.D3(sample.size = sample.size, n.sigma = 1)

    ,chart2.control.rules      = spc.rulesets.outside.limits()

    ,stat.lsl = NA
    ,stat.target = NA
    ,stat.usl = NA
   
    ,...
) {

#argg <- c(as.list(environment()), list(...))
#    print(argg)

ret <- list(
  description = "Individual and Moving Range Chart Information"
  ,parameter.individuals = individuals
  ,parameter.movingranges = movingranges
  ,parameter.sample.size = sample.size
  ,x.labels = x #label series
  ,chart1.series = individuals   #data series
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
    chart.series       = individuals, 
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

  ,chart2.series     = movingranges
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
    chart.series       = movingranges, 
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

if (any(!is.na(c(stat.lsl, stat.target, stat.usl)))) {
  #calculate capability
  ret$capability <- spc.capability.summary.normal.simple(
    stat.lsl = stat.lsl,
    stat.target = stat.target,
    stat.usl = stat.usl,
    process.center = mean(individuals),
    process.variability.estimate = (mean(na.omit(movingranges))/spc.constant.calculation.d2(2))^2,
    process.variability.overall = var(individuals),
    process.n = length(individuals),
    process.n.upper = ifelse(!is.na(stat.usl), sum(individuals > stat.usl),NA),
    process.n.lower = ifelse(!is.na(stat.lsl), sum(individuals < stat.lsl),NA)
  )
}

spc.chart.simple(
  x = x #label series
  
  ,chart1.series = individuals   #data series
  ,chart1.main     = chart1.main
  ,chart1.ylab     = chart1.ylab
  ,chart1.center.line = chart1.center.line
  ,chart1.control.limits.ucl = chart1.control.limits.ucl
  ,chart1.control.limits.lcl = chart1.control.limits.lcl
  ,chart1.is.control.violation = ret$chart1.is.control.violation$overall.results

  ,chart2.series     = movingranges
  ,chart2.main       = chart2.main
  ,chart2.ylab       = chart2.ylab
  ,chart2.center.line = chart2.center.line
  ,chart2.control.limits.ucl = chart2.control.limits.ucl 
  ,chart2.control.limits.lcl = chart2.control.limits.lcl 
  ,chart2.is.control.violation = ret$chart2.is.control.violation$overall.results
  ,...
)
    
  invisible(ret)

}
