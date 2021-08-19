#' Calculate Capability Measures - Cpm  
#' 
#' Calculate Cpm, a measure that combines the measure of variability and targeting relative to nominal specifications. Higher is better. 
#'
#' @param lower.specification Lower specification limit (if applicable)
#' @param upper.specification Upper specification limit (if applicable)
#' @param process.variability Estimate of process variability, expressed as variance
#' @param process.center Estimate of process center
#' @param nominal.center Nominal target for the process
#' @param n.sigma The number of standard deviations to use in the denominator of the calculation. 6 is recommended, but 5.15 has also been historically used by Automotive Industry Action Group (AIAG). 
#'
#' @return A scalar with computed Cpm. 

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