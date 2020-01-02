spc.controlviolation.nelson.1984.test3.trends.decreasing <- function(
    chart.series = NA,

    center.line = NA,

    control.limits.ucl = NA,
    zone.a.upper = NA,
    zone.ab.upper = NA,
    zone.bc.upper = NA,

    control.limits.lcl = NA,
    zone.a.lower = NA,
    zone.ab.lower = NA,
    zone.bc.lower = NA,

    point.count = 6,
    strictly.decreasing = T,

    ...
) {
    ret <- logical(0)
  
    run.lengths <- if (strictly.decreasing) {
        rle(diff(chart.series) < 0) 
    } else {
        rle(diff(chart.series) <= 0)  
    }
    
    i <- 1
    
    for (i in run.lengths$lengths) {
        is.run.violation <- i >= point.count
        ret <- c(ret, rep(is.run.violation, i))
    }
    
    ret<-c(ret[1],ret)
    ret[pmax(1,(which(ret == T)-1))]<-T

    ret[which(is.na(ret))] <- F

    ret 
}