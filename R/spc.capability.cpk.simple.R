#' Calculate Capability Measures - Cpk  
#' 
#' Calculate Cpk, a measure that compares the distance between process center and nearest specification limit to process variability. 
#' Lower process variability and larger distance between process center and nearest specification limit lead to higher values, which 
#' are generally considered better. 
#'
#' @param lower.specification Lower specification limit (if applicable)
#' @param upper.specification Upper specification limit (if applicable)
#' @param process.variability Estimate of process variability, expressed as variance
#' @param process.center Estimate of process center
#' @param n.sigma The number of standard deviations to use in the denominator of the calculation. 6 is recommended, but 5.15 has also been historically used by Automotive Industry Action Group (AIAG). 
#'
#' @return A scalar with computed Cp. 

spc.capability.cpk.simple <- function(lower.specification
                                      ,upper.specification
                                      ,process.variability #Usually Expressed as Variance
                                      ,process.center
                                      ,n.sigma = 6) {
  
  cpk <- NA
  
  cpk <- min(na.omit(c(spc.capability.cpU.simple(
    upper.specification
    ,process.center
    ,process.variability #Usually Expressed as Variance
    ,n.sigma
  ),
  spc.capability.cpL.simple(
    lower.specification
    ,process.center
    ,process.variability #Usually Expressed as Variance
    ,n.sigma)  
  )))

  cpk
}