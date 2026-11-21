#' Calculate Capability Measures - Cp  
#' 
#' Calculate Cp, a measure that compares specification spread with observed process process spread. 
#' Higher values indicate that more process 'widths' can fit into the specification limits.
#' This measure does not identify whether a process is 'targeted' within the specification limits.
#'
#' @param lower.specification Lower specification limit (if applicable)
#' @param upper.specification Upper specification limit (if applicable)
#' @param process.center Estimate of process center, used if specification limits are one-sided
#' @param process.natural.tolerance Estimate of process natural tolerance 
#'
#' @return A scalar with computed Cp. 
spc.capability.cp.simple <- function(lower.specification
                              ,upper.specification
                              ,process.center = NA
                              ,process.natural.tolerance) {

  cp <- NA 

  if (!is.na(lower.specification) & !is.na(upper.specification)) {
    cp <- (upper.specification-lower.specification)/process.natural.tolerance
  } else if (is.na(lower.specification) | is.na(upper.specification)) {
    cp <- (2*abs(process.center- na.omit(c(upper.specification,lower.specification))))/process.natural.tolerance 
  }

  cp
}