spc.capability.cpm.simple <- function(
  lower.specification
  ,upper.specification
  ,process.variability #Usually Expressed as Variance
  ,process.center
  ,nominal.center
  ,n.sigma = 6) {
  cpm <- NA

  if (!is.na(lower.specification) & !is.na(upper.specification)) {
   cpm <- (upper.specification - lower.specification)/(n.sigma*sqrt(process.variability + (process.center - nominal.center)^2))
  } else if (is.na(lower.specification) | is.na(upper.specification)) {
   cpm <- (2*abs(process.center- na.omit(c(upper.specification,lower.specification))))/(n.sigma*sqrt(process.variability + (process.center - nominal.center)^2)) 
  }

  cpm
  
}