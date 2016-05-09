spc.capability.cpm.simple <- function(lower.specification
                               ,upper.specification
                               ,process.variability #Usually Expressed as Variance
                               ,process.center
                               ,nominal.center
                               ,n.sigma = 6) {
  (upper.specification - lower.specification)/(n.sigma*sqrt(process.variability + (process.center - nominal.center)^2))
}