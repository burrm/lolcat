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