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