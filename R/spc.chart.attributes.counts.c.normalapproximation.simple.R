#Naming convention - variables charts
#          var/attr  Loc CL      Dispersion Input
#spc.chart.variables.mean   .and.range      .simple ()

spc.chart.attributes.counts.c.normalapproximation.simple <- function(
    counts
    ,x = 1:length(counts)

    ,chart1.main     = "Count Chart\n(Normal Approximation Limits)"
    ,chart1.ylab     = "Count"
    ,chart1.center.line = rep(mean(counts), length(counts))
    ,chart1.control.limits.ucl = chart1.center.line + 3*sqrt(chart1.center.line)
    ,chart1.zone.a.upper  = chart1.control.limits.ucl
    ,chart1.zone.ab.upper = chart1.center.line + 2*sqrt(chart1.center.line)
    ,chart1.zone.bc.upper = chart1.center.line + sqrt(chart1.center.line)


    ,chart1.control.limits.lcl = chart1.center.line - 3*sqrt(chart1.center.line)
    ,chart1.zone.a.lower  = chart1.control.limits.lcl
    ,chart1.zone.ab.lower = chart1.center.line - 2*sqrt(chart1.center.line)
    ,chart1.zone.bc.lower = chart1.center.line - sqrt(chart1.center.line)

    ,chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4()

    ,chart2.display = F

    ,...
) {

#argg <- c(as.list(environment()), list(...))
#    print(argg)

fn.clean <- function(x) {
    x[which(x < 0)] <- NA
    x
}

chart1.center.line <- rep_len(chart1.center.line, length(counts))
chart1.center.line <- fn.clean(chart1.center.line)

chart1.control.limits.ucl <- rep_len(chart1.control.limits.ucl, length(counts))
chart1.control.limits.ucl <- fn.clean(chart1.control.limits.ucl)

chart1.zone.a.upper  <- rep_len(chart1.zone.a.upper, length(counts))
chart1.zone.a.upper <- fn.clean(chart1.zone.a.upper)

chart1.zone.ab.upper <- rep_len(chart1.zone.ab.upper, length(counts))
chart1.zone.ab.upper <- fn.clean(chart1.zone.ab.upper)

chart1.zone.bc.upper <- rep_len(chart1.zone.bc.upper, length(counts))
chart1.zone.bc.upper <- fn.clean(chart1.zone.bc.upper)


chart1.control.limits.lcl <- rep_len(chart1.control.limits.lcl, length(counts))
chart1.control.limits.lcl <- fn.clean(chart1.control.limits.lcl)

chart1.zone.a.lower  <- rep_len(chart1.zone.a.lower, length(counts))
chart1.zone.a.lower <- fn.clean(chart1.zone.a.lower)

chart1.zone.ab.lower <- rep_len(chart1.zone.ab.lower, length(counts))
chart1.zone.ab.lower <- fn.clean(chart1.zone.ab.lower)

chart1.zone.bc.lower <- rep_len(chart1.zone.bc.lower, length(counts))
chart1.zone.bc.lower <- fn.clean(chart1.zone.bc.lower)

ret <- list(
  description = "Count (c) Chart Information"
  ,parameter.counts = counts
  ,x.labels = x #label series
  ,chart1.series = counts   #data series
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
    chart.series       = counts, 
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
  
  ,chart1.series = counts   #data series
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
