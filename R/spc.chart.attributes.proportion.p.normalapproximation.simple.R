#Naming convention - variables charts
#          var/attr  Loc CL      Dispersion Input
#spc.chart.variables.mean   .and.range      .simple ()

spc.chart.attributes.proportion.p.normalapproximation.simple <- function(
    proportions
    ,sample.size
    ,x = 1:length(proportions)

    ,chart1.main     = "Proportion Chart\n(Normal Approximation Limits)"
    ,chart1.ylab     = "Proportion"
    ,chart1.center.line = rep(mean(proportions), length(proportions))
    ,chart1.control.limits.ucl = chart1.center.line + 3*sqrt((chart1.center.line *(1-chart1.center.line)) /sample.size)
    ,chart1.zone.a.upper  = chart1.control.limits.ucl
    ,chart1.zone.ab.upper = chart1.center.line + 2*sqrt((chart1.center.line *(1-chart1.center.line)) /sample.size)
    ,chart1.zone.bc.upper = chart1.center.line + sqrt((chart1.center.line *(1-chart1.center.line)) /sample.size)


    ,chart1.control.limits.lcl = chart1.center.line - 3*sqrt((chart1.center.line *(1-chart1.center.line)) /sample.size)
    ,chart1.zone.a.lower  = chart1.control.limits.lcl
    ,chart1.zone.ab.lower = chart1.center.line - 2*sqrt((chart1.center.line *(1-chart1.center.line)) /sample.size)
    ,chart1.zone.bc.lower = chart1.center.line - sqrt((chart1.center.line *(1-chart1.center.line)) /sample.size)

    ,chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4()

    ,chart2.display = F

    ,...
) {

#argg <- c(as.list(environment()), list(...))
#    print(argg)

sample.size <- rep_len(sample.size, length(proportions))

chart1.center.line <- sapply(rep_len(chart1.center.line, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.control.limits.ucl <- sapply(rep_len(chart1.control.limits.ucl, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.zone.a.upper  <- sapply(rep_len(chart1.zone.a.upper, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.zone.ab.upper <- sapply(rep_len(chart1.zone.ab.upper, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.zone.bc.upper <- sapply(rep_len(chart1.zone.bc.upper, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })


chart1.control.limits.lcl <- sapply(rep_len(chart1.control.limits.lcl, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.zone.a.lower  <- sapply(rep_len(chart1.zone.a.lower, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.zone.ab.lower <- sapply(rep_len(chart1.zone.ab.lower, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })
chart1.zone.bc.lower <- sapply(rep_len(chart1.zone.bc.lower, length(proportions)), FUN = function(x) { ifelse(x > 1 | x < 0, NA, x) })

ret <- list(
  description = "Proportion (p) Chart Information"
  ,x.labels = x #label series
  ,parameter.proportions = proportions
  ,parameter.sample.size = sample.size
  ,chart1.series = proportions   #data series
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
    chart.series       = proportions, 
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

)

spc.chart.simple(
  x = x #label series
  
  ,chart1.series = proportions   #data series
  ,chart1.main     = chart1.main
  ,chart1.ylab     = chart1.ylab
  ,chart1.center.line = chart1.center.line
  ,chart1.control.limits.ucl = chart1.control.limits.ucl
  ,chart1.control.limits.lcl = chart1.control.limits.lcl
  ,chart1.is.control.violation = ret$chart1.is.control.violation$overall.results

  ,chart2.display = chart2.display
)
    
  invisible(ret)

}
