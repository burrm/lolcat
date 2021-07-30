spc.capability.summary.normal.simple <- function(
  stat.lsl = NA,                     #Lower specification limit
  stat.target = NA,                  #Nominal process center - target median/mean
  stat.usl = NA,                     #Upper specification limit
  process.center = NA,               #Estimate of process center - mean
  process.variability.estimate = NA, #Estimate of process variability, expressed as variance, usually estimated based on within-group variability
  process.variability.overall = NA,  #Estimate of process variability for performance measures, expressed as variance, usually estimated based on overall variability
  process.n.upper = NA,              #n above USL
  process.n.lower = NA,              #n below LSL
  process.n = NA,                    #total observed
  process.ppm.upper = round(1000000*process.n.upper/process.n), #observed PPM above USL
  process.ppm.lower = round(1000000*process.n.lower/process.n) #observed PPM below LSL
) {
  
  natural.tolerance.estimate <- 6*sqrt(process.variability.estimate)
  stat.cp <- spc.capability.cp.simple(
    lower.specification = stat.lsl, 
    upper.specification = stat.usl, 
    process.natural.tolerance = natural.tolerance.estimate
  )

  #print(paste0("stat.lsl                     = ", stat.lsl))
  #print(paste0("stat.target                  = ", stat.target))
  #print(paste0("stat.usl                     = ", stat.usl))
  #print(paste0("process.center               = ", process.center))
  #print(paste0("process.variability.estimate = ", process.variability.estimate))
  #print(paste0("process.variability.overall  = ", process.variability.overall))
  #print(paste0("process.n.upper              = ", process.n.upper))
  #print(paste0("process.n.lower              = ", process.n.lower))
  #print(paste0("process.n                    = ", process.n))
  #print(paste0("process.ppm.upper            = ", process.ppm.upper))
  #print(paste0("process.ppm.lower            = ", process.ppm.lower))

  stat.cpk <- spc.capability.cpk.simple(
    lower.specification = stat.lsl, 
    upper.specification = stat.usl,
    process.variability = process.variability.estimate, 
    process.center = process.center, 
    n.sigma = 6
  )

  stat.cpm <- spc.capability.cpm.simple(
    lower.specification = stat.lsl, 
    upper.specification = stat.usl, 
    process.variability = process.variability.estimate, 
    process.center = process.center, 
    nominal.center = stat.target, 
    n.sigma = 6
  )

  natural.tolerance.overall <- 6*sqrt(process.variability.overall)
  stat.pp <- spc.capability.cp.simple(
    lower.specification = LSL, 
    upper.specification = USL, 
    process.natural.tolerance = natural.tolerance.overall
  )

  stat.ppk <- spc.capability.cpk.simple(
    lower.specification = stat.lsl, 
    upper.specification = stat.usl,
    process.variability = process.variability.overall, 
    process.center = process.center, 
    n.sigma = 6
  )

  stat.ppm <- spc.capability.cpm.simple(
    lower.specification = stat.lsl, 
    upper.specification = stat.usl, 
    process.variability = process.variability.overall, 
    process.center = process.center, 
    nominal.center = stat.target, 
    n.sigma = 6
  )

  process.oos.total <- sum (na.omit(c(process.n.lower, process.n.upper)))
  process.ppm.total <- sum(na.omit(c(process.ppm.lower, process.ppm.upper)))
  
    
  ppm.lower <- round(
    1000000*pnorm(
      q = stat.lsl, 
      mean = process.center, 
      sd = sqrt(process.variability.estimate), 
      lower.tail = T)
  )
  
  ppm.upper <- round(
    1000000*pnorm(
      q = stat.usl, 
      mean = process.center, 
      sd = sqrt(process.variability.estimate), 
      lower.tail = F)
  )
  
  ppm.total <- sum(na.omit(c(ppm.lower,ppm.upper)))

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