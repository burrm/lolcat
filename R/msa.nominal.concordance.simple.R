#' Perform Comparison of Two Nominal Measurements
#' 
#' Evaluate statistical tests of agreement and symmetry for two measurements.
#'
#' @param measurement1 Vector - First operator's assessments ordered by part
#' @param measurement2 Vector - Second operator's assessments ordered by part
#' @param conf.level Confidence level to use for the statistical tests
#'
#' @return A data structure including all of the details of the tests. Use summary() to summarize results. 
msa.nominal.concordance.simple <- function(
  measurement1,
  measurement2,
  conf.level = .95
) {
  ret <- list()
  
  if(inherits(measurement1, "lolcat.msa.nominal.internalconsistency")) {
    measurement1 <- measurement1$mode
  }
  
  if(inherits(measurement2, "lolcat.msa.nominal.internalconsistency")) {
    measurement2 <- measurement2$mode
  }
  
  xt <- transform.independent.format.to.xt(measurement1, measurement2)
  
  ret$agreement <- cor.cohen.kappa.onesample(xt, conf.level = conf.level)
  ret$symmetry  <- cor.bowker.symmetry.1948(xt)
  
  ret
}