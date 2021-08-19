#' Calculate Capability Measures - CpU  
#' 
#' Calculate CpU, a measure that compares the distance between process center and upper specification limit to process variability. 
#' Lower process variability and larger distance between process center and upper specification limit lead to higher values, which 
#' are generally considered better. 
#'
#' @param upper.specification Upper specification limit (if applicable)
#' @param process.variability Estimate of process variability, expressed as variance
#' @param process.center Estimate of process center
#' @param n.sigma The number of standard deviations to use in the denominator of the calculation. 6 is recommended, but 5.15 has also been historically used by Automotive Industry Action Group (AIAG). 
#'
#' @return A scalar with computed CpU. 

spc.capability.cpU.simple <- function (
  upper.specification
  ,process.center
  ,process.variability #Usually Expressed as Variance
  ,n.sigma = 6
) {
  abs((upper.specification-process.center)/(n.sigma*sqrt(process.variability)/2)) 
}