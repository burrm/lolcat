spc.chart.variables.mean.and.meanstandarddeviation <- function(
    data, 
    sample, 
    
    chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4(),

    chart2.control.rules = spc.rulesets.outside.limits(),

    ...
) {
    out <- spc.preprocess.data(
        data = data
        ,sample = sample
        ,stat.n = T
        ,stat.mean = T
        ,stat.sd = T
    )

    spc.chart.variables.mean.and.meanstandarddeviation.simple(
        means = out$mean
        ,standard.deviations = out$sd
        ,sample.size = out$n
        ,x = out$g

        ,chart1.control.rules = chart1.control.rules
        ,chart2.control.rules = chart2.control.rules

        ,...
    )

}