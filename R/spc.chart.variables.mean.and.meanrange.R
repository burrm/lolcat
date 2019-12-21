spc.chart.variables.mean.and.meanrange <- function(
    data, 
    sample, 
    
    chart1.control.rules = list(
      outside.limits = spc.controlviolation.nelson.1984.test1.outside.zone.a,
      runs = spc.controlviolation.nelson.1984.test2.runs,
      trends = spc.controlviolation.nelson.1984.test3.trends,
      alternating = spc.controlviolation.nelson.1984.test4.alternating
    ),

    chart2.control.rules = list(
      outside.limits = spc.controlviolation.nelson.1984.test1.outside.zone.a
    ),

    ...
) {
    out <- spc.preprocess.data(
        data = data
        ,sample = sample
        ,stat.n = T
        ,stat.mean = T
        ,stat.range = T
    )

    spc.chart.variables.mean.and.meanrange.simple(
        means = out$mean
        ,ranges = out$range
        ,sample.size = out$n
        ,x = out$g

        ,chart1.control.rules = chart1.control.rules
        ,chart2.control.rules = chart2.control.rules

        ,...
    )

}