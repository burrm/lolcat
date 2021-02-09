#simple interface for two charts
spc.chart.simple <- function(
  x = 1:length(chart1.series) #label series
  
  ,chart1.display   = T
  ,chart1.series   #data series
  ,chart1.main     = "Location Chart"
  ,chart1.xlab     = "Time"
  ,chart1.xlim     = c(1,length(chart1.series))
  ,chart1.ylab     = "Criterion Measure"
  ,chart1.ylim     = c(min(c(chart1.series, chart1.center.line, chart1.control.limits.ucl, chart1.control.limits.lcl), na.rm = T)
                      ,max(c(chart1.series, chart1.center.line, chart1.control.limits.ucl, chart1.control.limits.lcl), na.rm = T)
                       )
  ,chart1.col      = ifelse(chart1.is.control.violation, "orange", "blue")
  ,chart1.line.col = rep("grey",length(x))
  ,chart1.line.lwd = NA
  ,chart1.pch      = ifelse(chart1.is.control.violation, 19, 19)
  ,chart1.point.cex = NA
  ,chart1.is.control.violation = rep(F, length(chart1.series)) #Is control violation
  ,chart1.center.line = rep(NA,length(x))
  ,chart1.center.line.col   = rep("lightblue",length(x))
  ,chart1.control.limits.ucl = rep(NA, length(x))
  ,chart1.control.limits.ucl.col = rep("orange", length(x))
  ,chart1.control.limits.lcl = rep(NA, length(x))
  ,chart1.control.limits.lcl.col = rep("orange", length(x))
  ,chart1.after.plot = function () {} #After chart 1 plot
  
  
  
  ,chart2.display   = T
  ,chart2.series    = chart1.series
  ,chart2.main      = "Variability Chart"
  ,chart2.xlab      = "Time"
  ,chart2.xlim      = chart1.xlim
  ,chart2.ylab      = "Variability"
  ,chart2.ylim      = c(min(c(chart2.series, chart2.center.line, chart2.control.limits.ucl, chart2.control.limits.lcl), na.rm = T)
                       ,max(c(chart2.series, chart2.center.line, chart2.control.limits.ucl, chart2.control.limits.lcl), na.rm = T)
                      )
  ,chart2.col       = ifelse(chart2.is.control.violation,"orange","blue")
  ,chart2.line.col  = rep("grey",length(x))
  ,chart2.line.lwd  = NA
  ,chart2.pch       = ifelse(chart2.is.control.violation,19,19)
  ,chart2.point.cex = NA
  ,chart2.is.control.violation = rep(F, length(chart2.series)) #Is control violation
  ,chart2.center.line = rep(NA,length(x))
  ,chart2.center.line.col   = rep("lightblue",length(x))
  ,chart2.control.limits.ucl = rep(NA, length(x))
  ,chart2.control.limits.ucl.col = rep("orange", length(x))
  ,chart2.control.limits.lcl = rep(NA, length(x))
  ,chart2.control.limits.lcl.col = rep("orange", length(x)) 
  ,chart2.after.plot = function () {} #After chart 2 plot
  
  ,combine.charts   = c("combine.charts", "separate", "leave.par.alone")
  ,...
  
) {
  par.backup <- par(no.readonly = T)
  
  if (is.na(chart1.line.lwd)) {
    chart1.line.lwd <- par()$lwd
  }

  if (is.na(chart1.point.cex)) {
    chart1.point.cex <- par()$cex
  }

  if (is.na(chart2.line.lwd)) {
    chart2.line.lwd <- par()$lwd
  }

  if (is.na(chart2.point.cex)) {
    chart2.point.cex <- par()$cex
  }

  if (combine.charts[1] == "leave.par.alone") {
    
  } else if (chart1.display & chart2.display & combine.charts[1] == "combine.charts") {
    par(mfrow=c(2,1))
    par(mar=c(4,4,2,2))
    par(cex.main=1)
  } else {
    par(mfrow=c(1,1))
  }
  
  
  if (chart1.display) {
    #normalize lengths for chart 1
    if (length(x) != length(chart1.col)) {
      chart1.col <- rep_len(chart1.col, length(x))
    }

    if (length(x) != length(chart1.line.col)) {
      chart1.line.col <- rep_len(chart1.line.col, length(x))
    }
    
    if (length(x) != length(chart1.center.line)) {
      chart1.center.line <- rep_len(chart1.center.line, length(x))
    }

    if (length(x) != length(chart1.center.line.col)) {
      chart1.center.line.col <- rep_len(chart1.center.line.col, length(x))
    }

    if (length(x) != length(chart1.pch)) {
      chart1.pch <- rep_len(chart1.pch, length(x))
    }

    if (length(x) != length(chart1.control.limits.ucl)) {
      chart1.control.limits.ucl <- rep_len(chart1.control.limits.ucl, length(x))
    }

    if (length(x) != length(chart1.control.limits.ucl.col)) {
      chart1.control.limits.ucl.col <- rep_len(chart1.control.limits.ucl.col, length(x))
    }

    if (length(x) != length(chart1.control.limits.lcl)) {
      chart1.control.limits.lcl <- rep_len(chart1.control.limits.lcl, length(x))
    }

    if (length(x) != length(chart1.control.limits.lcl.col)) {
      chart1.control.limits.lcl.col <- rep_len(chart1.control.limits.lcl.col, length(x))
    }

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
         lwd = chart1.line.lwd,
         ...)
    
    if (any(!is.na(chart1.center.line))) {
      sapply(1:length(x), FUN = function(i) {
        lines(
          c(i-.5,i+.5), 
          c(chart1.center.line[i], 
          chart1.center.line[i]), 
          col = chart1.center.line.col[i],
          lwd = chart1.line.lwd
        )

        if (i > 1 & all(!is.na(c(chart1.center.line[i-1], chart1.center.line[i])))) {
          lines(
            c(i-.5,i-.5), 
            c(chart1.center.line[i-1], 
            chart1.center.line[i]), 
            col = chart1.center.line.col[i-1],
            lwd = chart1.line.lwd)
        }
      })
      #lines(1:length(x), chart1.center.line, col = chart1.center.line.col)
    }
    
    if (any(!is.na(chart1.control.limits.ucl))) {
      sapply(1:length(x), FUN = function(i) {
        lines(
          c(i-.5,i+.5), 
          c(chart1.control.limits.ucl[i], chart1.control.limits.ucl[i]), 
          col = chart1.control.limits.ucl.col[i],
          lwd = chart1.line.lwd
        )

        if (i > 1 & all(!is.na(c(chart1.control.limits.ucl[i-1], chart1.control.limits.ucl[i])))) {
          lines(
            c(i-.5,i-.5), 
            c(chart1.control.limits.ucl[i-1], chart1.control.limits.ucl[i]), 
            col = chart1.control.limits.ucl.col[i-1],
            lwd = chart1.line.lwd
            )
        }
      })
      #lines(1:length(x), chart1.control.limits.ucl, col = chart1.control.limits.ucl.col, type="S")     
    }
    
    if (any(!is.na(chart1.control.limits.lcl))) {
      sapply(1:length(x), FUN = function(i) {
        lines(
          c(i-.5,i+.5), 
          c(chart1.control.limits.lcl[i], 
          chart1.control.limits.lcl[i]), 
          col = chart1.control.limits.lcl.col[i],
          lwd = chart1.line.lwd
        )
      
        if (i > 1 & all(!is.na(c(chart1.control.limits.lcl[i-1], chart1.control.limits.lcl[i])))) {
          lines(
            c(i-.5,i-.5), 
            c(chart1.control.limits.lcl[i-1], chart1.control.limits.lcl[i]), 
            col = chart1.control.limits.lcl.col[i-1],
            lwd = chart1.line.lwd
          )
        }
      })
      #lines(1:length(x), chart1.control.limits.lcl, col = chart1.control.limits.lcl.col, type="S")     
    }

    axis(1,at=1:length(x),labels = if (is.factor(x)) { levels(x)[x] } else {x})
    
    points(1:length(x)
           ,chart1.series
           ,col = chart1.col
           ,pch = chart1.pch
           ,cex = chart1.point.cex
           )
    
    chart1.after.plot()
  }
   
  
  if (chart2.display) {
    #normalize lengths for chart 2
    if (length(x) != length(chart2.col)) {
      chart2.col <- rep_len(chart2.col, length(x))
    }

    if (length(x) != length(chart2.line.col)) {
      chart2.line.col <- rep_len(chart2.line.col, length(x))
    }
    
    if (length(x) != length(chart2.center.line)) {
      chart2.center.line <- rep_len(chart2.center.line, length(x))
    }

    if (length(x) != length(chart2.center.line.col)) {
      chart2.center.line.col <- rep_len(chart2.center.line.col, length(x))
    }

    if (length(x) != length(chart2.pch)) {
      chart2.pch <- rep_len(chart2.pch, length(x))
    }

    if (length(x) != length(chart2.control.limits.ucl)) {
      chart2.control.limits.ucl <- rep_len(chart2.control.limits.ucl, length(x))
    }

    if (length(x) != length(chart2.control.limits.ucl.col)) {
      chart2.control.limits.ucl.col <- rep_len(chart2.control.limits.ucl.col, length(x))
    }

    if (length(x) != length(chart2.control.limits.lcl)) {
      chart2.control.limits.lcl <- rep_len(chart2.control.limits.lcl, length(x))
    }

    if (length(x) != length(chart2.control.limits.lcl.col)) {
      chart2.control.limits.lcl.col <- rep_len(chart2.control.limits.lcl.col, length(x))
    }


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
         lwd = chart2.line.lwd,
         ...)
    
    
    if (any(!is.na(chart2.center.line))) {
      sapply(1:length(x), FUN = function(i) {
        lines(
          c(i-.5,i+.5), 
          c(chart2.center.line[i], chart2.center.line[i]), 
          col = chart2.center.line.col[i],
          lwd = chart2.line.lwd
          )

        if (i > 1 & all(!is.na(c(chart2.center.line[i-1], chart2.center.line[i])))) {
          lines(
            c(i-.5,i-.5), 
            c(chart2.center.line[i-1], chart2.center.line[i]), 
            col = chart2.center.line.col[i-1],
            lwd = chart2.line.lwd
            )
        }
      })
      #lines(1:length(x), chart2.center.line, col = chart2.center.line.col)
    }
    
    if (any(!is.na(chart2.control.limits.ucl))) {
      sapply(1:length(x), FUN = function(i) {
        lines(
          c(i-.5,i+.5), 
          c(chart2.control.limits.ucl[i], chart2.control.limits.ucl[i]), 
          col = chart2.control.limits.ucl.col[i],
          lwd = chart2.line.lwd
        )

        if (i > 1 & all(!is.na(c(chart2.control.limits.ucl[i-1], chart2.control.limits.ucl[i])))) {
          lines(
            c(i-.5,i-.5), 
            c(chart2.control.limits.ucl[i-1], chart2.control.limits.ucl[i]), 
            col = chart2.control.limits.ucl.col[i-1],
            lwd = chart2.line.lwd
            )
        }
      })
      #lines(1:length(x), chart2.control.limits.ucl, col = chart2.control.limits.ucl.col, type="S")     
    }
    
    if (any(!is.na(chart2.control.limits.lcl))) {
      sapply(1:length(x), FUN = function(i) {
        lines(
          c(i-.5,i+.5), 
          c(chart2.control.limits.lcl[i], chart2.control.limits.lcl[i]), 
          col = chart2.control.limits.lcl.col[i],
          lwd = chart2.line.lwd
        )
      
        if (i > 1 & all(!is.na(c(chart2.control.limits.lcl[i-1], chart2.control.limits.lcl[i])))) {
          lines(
            c(i-.5,i-.5), 
            c(chart2.control.limits.lcl[i-1], chart2.control.limits.lcl[i]), 
            col = chart2.control.limits.lcl.col[i-1],
            lwd = chart2.line.lwd
          )
        }
      })
      #lines(1:length(x), chart2.control.limits.lcl, col = chart2.control.limits.lcl.col, type="S")     
    }
    
    axis(1,at=1:length(x),labels = if (is.factor(x)) { levels(x)[x] } else {x})
    
    points(1:length(x)
           ,chart2.series
           ,col = chart2.col
           ,pch = chart2.pch
           ,cex = chart2.point.cex

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