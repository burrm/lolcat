spc.individuals.chart<-function(x
                                #,set = rep(1,length(x))
                                ,xchart.center.line.fn = spc.center.line.mean
                                ,mrchart.center.line.fn = spc.center.line.mean.moving.range

                                ,xchart.control.limits.fn = spc.control.limits.mean.moving.range
                                ,mrchart.control.limits.fn = spc.control.limits.dispersion.mean.moving.range

                                ,n.sigma = 3
                                
                                ,info.xchart     = T
                                ,display.xchart  = T
                                ,xchart.main     = "Individuals Chart"
                                ,xchart.xlab     = "Time"
                                ,xchart.ylab     = "Criterion Measure"
                                ,xchart.col      = c("blue", "orange")
                                ,xchart.line.col = "grey"
                                ,xchart.pch      = c(19,19)
                                ,xchart.rules    = .spc.default.x.chart.rules
                                ,xchart.center.line.col   = "lightblue"
                                ,xchart.control.limit.col = "orange"
                                
                                ,info.mrchart     = T
                                ,display.mrchart  = T
                                ,mrchart.main     = "Moving Range Chart"
                                ,mrchart.xlab     = "Time"
                                ,mrchart.ylab     = "Moving Range"
                                ,mrchart.col      = c("blue", "orange")
                                ,mrchart.line.col = "grey"
                                ,mrchart.pch      = c(19,19)
                                ,mrchart.rules    = .spc.default.mr.chart.rules
                                ,mrchart.center.line.col   = "lightblue"
                                ,mrchart.control.limit.col = "orange"
                                
                                ,combine.charts   = c("separate", "combine.charts")
                                
                                ,...) {
  par.backup <- par()

  if (display.xchart & display.mrchart & combine.charts[1] == "combine.charts") {
    par(mfrow=c(2,1))
  } else {
    par(mfrow=c(1,1))
  }
  
  
  mr.series <- c(NA,abs(diff(x)))
  
  xchart.center.line <- xchart.center.line.fn(x)
  mrchart.center.line <- mrchart.center.line.fn(x)  

  xchart.limits <- xchart.control.limits.fn(center.line = xchart.center.line,
                                            standard.error = mrchart.center.line)
  
  mrchart.limits <- mrchart.control.limits.fn(standard.error = mrchart.center.line)
  
  xchart.ylim <- c(min(na.omit(c(x, xchart.limits$LCL))), max(na.omit(c(x, xchart.limits$UCL))))
  mrchart.ylim <- c(min(na.omit(c(mr.series, mrchart.limits$LCL))), max(na.omit(c(mr.series, mrchart.limits$UCL))))
  

  if (length(xchart.rules) > 0 & !all(is.na(xchart.rules))) {
    xchart.violations <- matrix(xchart.rules[[1]](x = x
                                                  ,center.line = xchart.center.line
                                                  ,standard.error = mrchart.center.line
                                                  ,upper.control.limit = xchart.limits$UCL
                                                  ,lower.control.limit = xchart.limits$LCL
                                                  )
                                ,nrow=length(x)
                                ,ncol = 1)
     
    if (length(xchart.rules) > 1) {
      for (i in 2:length(xchart.rules)) {
        xchart.violations <- cbind(xchart.violations, 
                                   xchart.rules[[i]](x = x
                                                     ,center.line = xchart.center.line
                                                     ,standard.error = mrchart.center.line
                                                     ,upper.control.limit = xchart.limits$UCL
                                                     ,lower.control.limit = xchart.limits$LCL
        ))
      }
    }
  }
    
  xchart.viol <- rep(1,length(x)) 
  
  xchart.viol <- xchart.viol + ifelse(apply(xchart.violations,1,FUN=sum) > 0, 1, 0)
  
  if (display.xchart) {
    plot(1:length(x), 
         x, 
         main=xchart.main, 
         xlab = xchart.xlab, 
         ylab = xchart.ylab, 
         ylim = xchart.ylim,
         type = "l", 
         col = xchart.line.col, 
         xaxt="n",
         ...)
    
    if (!is.na(xchart.center.line)) {
      abline(h=xchart.center.line, col = xchart.center.line.col)
    }
    
    limits<-c(xchart.limits$UCL, xchart.limits$LCL)
    limits <- na.omit(limits)
    
    if (length(limits > 0)) {
      abline(h=limits, col = xchart.control.limit.col)
    }
    
    axis(1,at=1:length(x),labels = 1:length(x))
    
    points(1:length(x), x, col = xchart.col[xchart.viol], pch = xchart.pch[xchart.viol])
  }


  
  
  
  
  
  
  if (length(mrchart.rules) > 0 & !all(is.na(mrchart.rules))) {
    mrchart.violations <- matrix(mrchart.rules[[1]](x = mr.series
                                                  ,center.line = mrchart.center.line
                                                  ,standard.error = mrchart.center.line #TODO...
                                                  ,upper.control.limit = mrchart.limits$UCL
                                                  ,lower.control.limit = mrchart.limits$LCL
    )
    ,nrow=length(x)
    ,ncol = 1)
    
    if (length(mrchart.rules) > 1) {
      for (i in 2:length(mrchart.rules)) {
        mrchart.violations <- cbind(mrchart.violations, 
                                   mrchart.rules[[i]](x = mr.series
                                                     ,center.line = mrchart.center.line
                                                     ,standard.error = mrchart.center.line #TODO...
                                                     ,upper.control.limit = mrchart.limits$UCL
                                                     ,lower.control.limit = mrchart.limits$LCL
                                   ))
      }
    }
  }
  
  mrchart.viol <- rep(1,length(mr.series)) 
  
  mrchart.viol <- mrchart.viol + ifelse(apply(mrchart.violations,1,FUN=sum) > 0, 1, 0)  

  if (display.mrchart) {
    plot(1:length(mr.series), 
         mr.series, 
         main=mrchart.main, 
         xlab = mrchart.xlab, 
         ylab = mrchart.ylab, 
         ylim = mrchart.ylim,
         type = "l", 
         col = mrchart.line.col, 
         xaxt="n",
         ...)
    
    if (!is.na(mrchart.center.line)) {
      abline(h=mrchart.center.line, col = mrchart.center.line.col)
    }
    
    limits<-c(mrchart.limits$UCL, mrchart.limits$LCL)
    limits <- na.omit(limits)
    
    if (length(limits > 0)) {
      abline(h=limits, col = mrchart.control.limit.col)
    }
    
    axis(1,at=1:length(mr.series),labels = 1:length(mr.series))
    
    points(1:length(mr.series), mr.series, col = mrchart.col[mrchart.viol], pch = mrchart.pch[mrchart.viol])
  } 
  
  
  
  
   
  
  
  
  
  oldw <- getOption("warn")
  options(warn = -1)
  par(par.backup)
  options(warn = oldw)
  
}
