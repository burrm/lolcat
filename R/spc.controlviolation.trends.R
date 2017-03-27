spc.controlviolation.trend.positive <- function(x, trend.length=6, strict = T, ...) {
  ret <- logical(0)
  
  run.lengths <- if (strict) {
    rle(diff(x) > 0) 
  } else {
    rle(diff(x) >= 0)  
  }
  
  i <- 1
  
  for (i in run.lengths$lengths) {
    is.run.violation <- i >= trend.length
    ret <- c(ret, rep(is.run.violation, i))
  }
  
  ret<-c(ret[1],ret)
  ret[pmax(1,(which(ret == T)-1))]<-T
  
  ret
}


spc.controlviolation.trend.negative <- function(x, trend.length=6, strict = T, ...) {
  ret <- logical(0)
  
  run.lengths <- if (strict) {
    rle(diff(x) < 0) 
  } else {
    rle(diff(x) <= 0)  
  }
  
  i <- 1
  
  for (i in run.lengths$lengths) {
    is.run.violation <- i >= trend.length
    ret <- c(ret, rep(is.run.violation, i))
  }
  
  ret<-c(ret[1],ret)
  ret[pmax(1,(which(ret == T)-1))]<-T
  
  
  ret
}

spc.controlviolation.trends <- function(x, trend.length=6, strict = T, ...) {
  p <- spc.controlviolation.trend.positive(x = x,trend.length = trend.length, strict = strict, ...)
  n <- spc.controlviolation.trend.negative(x = x,trend.length = trend.length, strict = strict, ...)
  n | p
}