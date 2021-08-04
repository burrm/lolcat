spc.chart.variables.mean.and.meanstandarddeviation <- function(
    data, 
    sample, 
    
    chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4(),

    chart2.control.rules = spc.rulesets.outside.limits(),

    stat.lsl = NA,
    stat.target = NA,
    stat.usl = NA,

    ...
) {
  
  out <- spc.preprocess.data(
    data = data
    ,sample = sample
    ,stat.n = T
    ,stat.mean = T
    ,stat.sd = T
  )

  ret <- spc.chart.variables.mean.and.meanstandarddeviation.simple(
    means = out$mean
    ,standard.deviations = out$sd
    ,sample.size = out$n
    ,x = out$g

    ,chart1.control.rules = chart1.control.rules
    ,chart2.control.rules = chart2.control.rules

    ,...
  )

  if (any(!is.na(c(stat.lsl, stat.target, stat.usl)))) {
    #calculate capability
    ret$capability <- spc.capability.summary.normal.simple(
      stat.lsl = stat.lsl,
      stat.target = stat.target,
      stat.usl = stat.usl,
      process.center = mean(data),
      process.variability.estimate = (mean(ret$parameter.standard.deviations)/spc.constant.calculation.c4(median(out$n)))^2,
      process.variability.overall = var(data),
      process.n = length(data),
      process.n.upper = ifelse(!is.na(stat.usl), sum(data > stat.usl),NA),
      process.n.lower = ifelse(!is.na(stat.lsl), sum(data < stat.lsl),NA)
    )
  }
  
  ret

}