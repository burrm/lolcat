spc.capability.summary.ungrouped.nonnormal.simple.R <- function(
  stat.lsl = NA,                   #Lower specification limit
  stat.target = NA,                #Nominal process center - target median/mean
  stat.usl = NA,                   #Upper specification limit

  stat.lsl.capability    = stat.lsl,    #Lower specification limit - overridden for capability
  stat.target.capability = stat.target, #Nominal process center - target median/mean - overridden for capability
  stat.usl.capability    = stat.usl,    #Upper specification limit - overridden for capability

  stat.lsl.performance    = stat.lsl,    #Lower specification limit - overridden for performance measures
  stat.target.performance = stat.target, #Nominal process center - target median/mean - overridden for performance measures
  stat.usl.performance    = stat.usl,    #Upper specification limit - overridden for performance measures

  process.center.capability = NA,  #Estimate of process center - median
  process.center.performance = NA, #Estimate of process center - mean
  process.variability = NA,        #Estimate of process variability, expressed as variance, performance measures only
  process.n.upper = NA,            #n above USL
  process.n.lower = NA,            #n below LSL
  process.n = NA,                  #total observed
  process.ppm.upper = round(1000000*process.n.upper/process.n), #observed PPM above USL
  process.ppm.lower = round(1000000*process.n.lower/process.n), #observed PPM below LSL
  natural.tolerance = NA,          #distribution natural tolerance, capability measures only
  p.lower = NA,                    #distribution area below LSL
  p.upper = NA                     #distribution area above USL
) {
  
  if (is.na(stat.lsl.capability) | is.na(stat.usl.capability)) {
    stat.sl.capability <- na.omit(c(stat.lsl.capability, stat.usl.capability))
    stat.sl.performance <- na.omit(c(stat.lsl.performance, stat.usl.performance))

    #Cp
    stat.cp <- spc.capability.cp.simple(
      lower.specification = stat.lsl.capability,
      upper.specification = stat.usl.capability,
      process.center = process.center.capability,
      process.natural.tolerance = natural.tolerance
    )

    #Cpk, need lesser of center - lsl and usl - center
    stat.cpk <- NA
    lower.tail <- NA
    p.to.use <- NA
        
    if (is.na(stat.usl)) {
        p.to.use   <- p.lower
        lower.tail <- T
    } else {
        p.to.use <- p.upper
        lower.tail <- F
    }

    z <- qnorm(
        p = p.to.use,
        lower.tail = lower.tail
    )

    stat.cpk <- abs(z)/3
  

    #Cpm
    stat.cpm <- spc.capability.cpm.simple(
        lower.specification = stat.lsl.capability,
        nominal.center      = stat.target.capability, 
        upper.specification = stat.usl.capability, 
        process.variability = (natural.tolerance/6)^2, 
        process.center = process.center.capability,  
        n.sigma = 6
    )

    s <- sqrt(process.variability)
    
    #Pp
    stat.pp <- spc.capability.cp.simple(
      lower.specification = stat.lsl.performance, 
      upper.specification = stat.usl.performance, 
      process.center = process.center.performance,
      process.natural.tolerance = 6*s
    )

    #Ppk
    stat.ppk <- (2*abs(process.center.performance-stat.sl.performance)/(6*s))

    #Ppm
    stat.ppm <- spc.capability.cpm.simple(
      lower.specification = stat.lsl.performance, 
      nominal.center      = stat.target.performance,
      upper.specification = stat.usl.performance, 
      process.variability = s^2, 
      process.center      = process.center.performance, 
      n.sigma             = 6
    )  

    ppm.lower <- round(1000000*p.lower)
    ppm.upper <- round(1000000*p.upper)
    ppm.total <- sum(na.omit(c(ppm.lower,ppm.upper)))

    process.ppm.total <- sum(na.omit(process.ppm.upper, process.ppm.lower))
    process.oos.total <- sum(na.omit(process.n.upper,   process.n.lower))

    
  }
  else {

    #Cp
    stat.cp <- spc.capability.cp.simple(
      lower.specification = stat.lsl.capability, 
      upper.specification = stat.usl.capability, 
      process.natural.tolerance = natural.tolerance
    )
          
    #Cpk, need lesser of center - lsl and usl - center
    stat.cpk <- NA
    if (process.center.capability >= stat.lsl & process.center.capability <= stat.usl) {
        closer = c(process.center.capability - stat.lsl, stat.usl - process.center.capability)
        lower.tail <- NA
        p.to.use <- NA
        
        if (closer[1] < closer[2]) {
            p.to.use   <- p.lower
            lower.tail <- T
        } else {
            p.to.use <- p.upper
            lower.tail <- F
        }

        z <- qnorm(
            p = p.to.use,
            lower.tail = lower.tail
        )

        stat.cpk <- abs(z)/3
    }
    #Cpm
    stat.cpm <- spc.capability.cpm.simple(
        lower.specification = stat.lsl.capability,
        nominal.center      = stat.target.capability, 
        upper.specification = stat.usl.capability, 
        process.variability = (natural.tolerance/6)^2, 
        process.center = process.center.capability,  
        n.sigma = 6
    )

    s <- sqrt(process.variability)
    
    #Pp
    stat.pp <- spc.capability.cp.simple(
      lower.specification = stat.lsl.performance, 
      upper.specification = stat.usl.performance, 
      process.center = process.center.performance,
      process.natural.tolerance = 6*s
    )

    #Ppk
    stat.ppk <- spc.capability.cpk.simple(
      lower.specification = stat.lsl.performance, 
      upper.specification = stat.usl.performance,
      process.variability = s^2, 
      process.center      = process.center.performance, 
      n.sigma             = 6
    )

    #Ppm
    stat.ppm <- spc.capability.cpm.simple(
      lower.specification = stat.lsl.performance, 
      nominal.center      = stat.target.performance,
      upper.specification = stat.usl.performance, 
      process.variability = s^2, 
      process.center = process.center.performance, 
      n.sigma = 6
    )  

    ppm.lower <- round(1000000*p.lower)
    ppm.upper <- round(1000000*p.upper)
    ppm.total <- ppm.lower+ppm.upper

    process.ppm.total <- sum(na.omit(process.ppm.upper, process.ppm.lower))
    process.oos.total <- sum(na.omit(process.n.upper,   process.n.lower))

  }

  output.table <- data.frame(
    statistic = c(
      "Cp", 
      "Cpk", 
      "Cpm", 
      "Pp", 
      "Ppk", 
      "Ppm",
      "Obs. n / PPM (lower)",
      "Obs. n / PPM (upper)",
      "Obs. n / PPM (total)",
      "Pot. PPM (lower)", 
      "Pot. PPM (upper)", 
      "Pot. PPM (total)"),
    eq = c(
      "=", 
      "=", 
      "=", 
      "=", 
      "=", 
      "=",
      "=",
      "=",
      "=",
      "=", 
      "=", 
      "="),
    n= c(
      "",
      "",
      "",
      "",
      "",
      "",
      process.n.lower,
      process.n.upper,
      process.oos.total,
      "",
      "",
      ""
    ),
    value = c(
      stat.cp,
      stat.cpk,
      stat.cpm,
      stat.pp,
      stat.ppk,
      stat.ppm,
      process.ppm.lower,
      process.ppm.upper,
      process.ppm.total,
      ppm.lower,
      ppm.upper,
      ppm.total
    ),
    stringsAsFactors = F
  )

  output.table
}