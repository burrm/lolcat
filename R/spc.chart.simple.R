#simple interface for two charts
spc.chart.simple <- function(
  x = 1:length(chart1.series) #label series
  
  ,chart1.display   = T
  ,chart1.series   #data series
  ,chart1.main     = "Location Chart"
  ,chart1.xlab     = "Time"
  ,chart1.xlim     = c(1,length(chart1.series))
  ,chart1.ylab     = "Criterion Measure"
  ,chart1.ylim     = c(min(c(chart1.series, chart1.center.line, chart1.control.limits), na.rm = T)
                      ,max(c(chart1.series, chart1.center.line, chart1.control.limits), na.rm = T)
                       )
  ,chart1.col      = c("blue", "orange")
  ,chart1.line.col = "grey"
  ,chart1.pch      = c(19,19)
  ,chart1.is.control.violation = rep(F, length(chart1.series)) #Is control violation
  ,chart1.center.line = NA
  ,chart1.center.line.col   = "lightblue"
  ,chart1.control.limits = c(NA, NA)
  ,chart1.control.limits.col = c("orange", "orange") #(Lim1, Lim2)
  ,chart1.after.plot = function () {} #After chart 1 plot
  
  
  
  ,chart2.display   = T
  ,chart2.series    = chart1.series
  ,chart2.main      = "Variability Chart"
  ,chart2.xlab      = "Time"
  ,chart2.xlim      = chart1.xlim
  ,chart2.ylab      = "Variability"
  ,chart2.ylim      = c(min(c(chart2.series, chart2.center.line, chart2.control.limits), na.rm = T)
                       ,max(c(chart2.series, chart2.center.line, chart2.control.limits), na.rm = T)
                      )
  ,chart2.col       = c("blue", "orange")
  ,chart2.line.col  = "grey"
  ,chart2.pch       = c(19,19)
  ,chart2.is.control.violation = rep(F, length(chart2.series)) #Is control violation
  ,chart2.center.line = NA
  ,chart2.center.line.col   = "lightblue"
  ,chart2.control.limits.col = c("orange", "orange") #(Lim1, Lim2)
  ,chart2.control.limits = c(NA,NA) 
  ,chart2.after.plot = function () {} #After chart 2 plot
  
  ,combine.charts   = c("combine.charts", "separate", "leave.par.alone")
  ,...
  
) {
  par.backup <- par(no.readonly = T)
  

  if (combine.charts[1] == "leave.par.alone") {
    
  } else if (chart1.display & chart2.display & combine.charts[1] == "combine.charts") {
    par(mfrow=c(2,1))
    par(mar=c(4,4,2,2))
    par(cex.main=1)
  } else {
    par(mfrow=c(1,1))
  }
  
  
  if (chart1.display) {
    plot(1:length(x), 
         chart1.series, 
         main = chart1.main, 
         xlab = chart1.xlab,
         xlim = chart1.xlim,
         ylab = chart1.ylab, 
         ylim = chart1.ylim,
         type = "l", 
         col = chart1.line.col, 
         xaxt="n",
         ...)
    
    if (!is.na(chart1.center.line)) {
      abline(h=chart1.center.line, col = chart1.center.line.col)
    }
    
    for (i in 1:length(chart1.control.limits)) {
      if (!is.na(chart1.control.limits[i])) {
        abline(
          h=chart1.control.limits[i]
          ,col = chart1.control.limits.col[i]
        )
      }
    }
    
    axis(1,at=1:length(x),labels = if (is.factor(x)) { levels(x)[x] } else {x})
    
    points(1:length(x)
           ,chart1.series
           ,col = chart1.col[as.numeric(chart1.is.control.violation)+1]
           ,pch = chart1.pch[as.numeric(chart1.is.control.violation)+1]
           )
    
    chart1.after.plot()
  }
   
  
  if (chart2.display) {
    plot(1:length(x), 
         chart2.series, 
         main = chart2.main, 
         xlab = chart2.xlab,
         xlim = chart2.xlim,
         ylab = chart2.ylab, 
         ylim = chart2.ylim,
         type = "l", 
         col = chart2.line.col, 
         xaxt="n",
         ...)
    
    if (!is.na(chart2.center.line)) {
      abline(h=chart2.center.line, col = chart2.center.line.col)
    }
    
    for (i in 1:length(chart2.control.limits)) {
      if (!is.na(chart2.control.limits[i])) {
        abline(
          h=chart2.control.limits[i]
          ,col = chart2.control.limits.col[i]
        )
      }
    }
    
    axis(1,at=1:length(x),labels = if (is.factor(x)) { levels(x)[x] } else {x})
    
    points(1:length(x)
           ,chart2.series
           ,col = chart2.col[as.numeric(chart2.is.control.violation)+1]
           ,pch = chart2.pch[as.numeric(chart2.is.control.violation)+1]
    )
    
    chart2.after.plot()
  }
  
  if (combine.charts[1] != "leave.par.alone") {
    oldw <- getOption("warn")
    options(warn = -1)
    par(par.backup)
    options(warn = oldw)
  }
}