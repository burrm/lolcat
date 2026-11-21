spc.controlviolation.nelson.1984.test2.runs.below <- function(
    chart.series = NA,

    center.line = NA,

    #control.limits.ucl = NA,
    #zone.a.upper = NA,
    #zone.ab.upper = NA,
    #zone.bc.upper = NA,

    #control.limits.lcl = NA,
    #zone.a.lower = NA,
    #zone.ab.lower = NA,
    #zone.bc.lower = NA,

    point.count = 9,

    ...
) {
  
    ret <- logical(0)

    observed.run.lengths <- rle(chart.series < center.line)
    
    for (i in 1:length(observed.run.lengths$lengths)) {
        is.run.violation <- observed.run.lengths$lengths[i] >= point.count & observed.run.lengths$values[i]
        ret <- c(ret, rep(is.run.violation, observed.run.lengths$lengths[i]))
    }

    ret[which(is.na(ret))] <- F

    ret  
}