#Naming convention - variables charts
#          var/attr  Loc CL      Dispersion Input
#spc.chart.variables.mean   .and.range      .simple ()

spc.chart.attributes.proportion.np.binomialdistribution.simple <- function(
    nonconforming
    ,sample.size
    ,x = 1:length(nonconforming)

    ,chart1.main     = "Proportion Chart\n(Binomial Distribution Limits)"
    ,chart1.ylab     = "Non-Conforming"
    ,chart1.center.line = rep(mean(nonconforming), length(nonconforming))
    ,chart1.control.limits.ucl = NA
    ,chart1.zone.a.upper  = NA
    ,chart1.zone.ab.upper = NA
    ,chart1.zone.bc.upper = NA


    ,chart1.control.limits.lcl = NA
    ,chart1.zone.a.lower  = NA
    ,chart1.zone.ab.lower = NA
    ,chart1.zone.bc.lower = NA

    ,chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4()

    ,chart2.display = F

    ,...
) {

#argg <- c(as.list(environment()), list(...))
#    print(argg)

sample.size <- rep_len(sample.size, length(nonconforming))

if (is.na(chart1.control.limits.ucl) | is.na(chart1.control.limits.lcl)) {
    tmp <- natural.tolerance.binom.simple(size = sample.size, prob = chart1.center.line/sample.size)
    
    if (is.na(chart1.control.limits.ucl)) {
        chart1.control.limits.ucl <- (tmp$upper.limit -.5)
    }

    if (is.na(chart1.control.limits.lcl)) {
        chart1.control.limits.lcl <- (tmp$lower.limit + .5)
    }
}

if (is.na(chart1.zone.a.upper)) {
    chart1.zone.a.upper <- chart1.control.limits.ucl
}

if (is.na(chart1.zone.a.lower)) {
    chart1.zone.a.lower <- chart1.control.limits.lcl
}

if (is.na(chart1.zone.ab.upper) | is.na(chart1.zone.ab.lower)) {
    tmp <- natural.tolerance.binom.simple(size = sample.size, prob = chart1.center.line/sample.size, total.area = 2*pnorm(-2))
    
    if (is.na(chart1.zone.ab.upper)) {
        chart1.zone.ab.upper <- (tmp$upper.limit - .5)
    }

    if (is.na(chart1.zone.ab.lower)) {
        chart1.zone.ab.lower <- (tmp$lower.limit + .5)
    }
}

if (is.na(chart1.zone.bc.upper) | is.na(chart1.zone.bc.lower)) {
    tmp <- natural.tolerance.binom.simple(size = sample.size, prob = chart1.center.line/sample.size, total.area = 2*pnorm(-1))
    
    if (is.na(chart1.zone.bc.upper)) {
        chart1.zone.bc.upper <- (tmp$upper.limit - .5)
    }

    if (is.na(chart1.zone.bc.lower)) {
        chart1.zone.bc.lower <- (tmp$lower.limit + .5)
    }
}

fn.clean <- function(x) {
    x[which(x > sample.size | x < 0)] <- NA
    x
}

chart1.center.line <- rep_len(chart1.center.line, length(nonconforming))
chart1.center.line <- fn.clean(chart1.center.line)

chart1.control.limits.ucl <- rep_len(chart1.control.limits.ucl, length(nonconforming))
chart1.control.limits.ucl <- fn.clean(chart1.control.limits.ucl)

chart1.zone.a.upper  <- rep_len(chart1.zone.a.upper, length(nonconforming))
chart1.zone.a.upper <- fn.clean(chart1.zone.a.upper)

chart1.zone.ab.upper <- rep_len(chart1.zone.ab.upper, length(nonconforming))
chart1.zone.ab.upper <- fn.clean(chart1.zone.ab.upper)

chart1.zone.bc.upper <- rep_len(chart1.zone.bc.upper, length(nonconforming))
chart1.zone.bc.upper <- fn.clean(chart1.zone.bc.upper)


chart1.control.limits.lcl <- rep_len(chart1.control.limits.lcl, length(nonconforming))
chart1.control.limits.lcl <- fn.clean(chart1.control.limits.lcl)

chart1.zone.a.lower  <- rep_len(chart1.zone.a.lower, length(nonconforming))
chart1.zone.a.lower <- fn.clean(chart1.zone.a.lower)

chart1.zone.ab.lower <- rep_len(chart1.zone.ab.lower, length(nonconforming))
chart1.zone.ab.lower <- fn.clean(chart1.zone.ab.lower)

chart1.zone.bc.lower <- rep_len(chart1.zone.bc.lower, length(nonconforming))
chart1.zone.bc.lower <- fn.clean(chart1.zone.bc.lower)

ret <- list(
  description = "Proportion (np) Chart Information"
  ,parameter.nonconforming = nonconforming
  ,parameter.sample.size = sample.size
  ,x.labels = x #label series
  ,chart1.series = nonconforming   #data series
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
    chart.series       = nonconforming, 
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
  
  ,chart1.series = nonconforming   #data series
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
