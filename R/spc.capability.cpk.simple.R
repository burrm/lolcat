spc.capability.cpk.simple <- function(lower.specification
                                      ,upper.specification
                                      ,process.variability #Usually Expressed as Variance
                                      ,process.center
                                      ,n.sigma = 6) {
  
  min(spc.capability.cpU.simple(
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
  )
}

spc.capability.cpU.simple <- function (upper.specification
                                ,process.center
                                ,process.variability #Usually Expressed as Variance
                                ,n.sigma = 6
) {
  abs((upper.specification-process.center)/(n.sigma*sqrt(process.variability)/2)) 
}

spc.capability.cpL.simple <- function (lower.specification
                                ,process.center
                                ,process.variability #Usually Expressed as Variance
                                ,n.sigma = 6
) {
  abs((process.center-lower.specification)/(n.sigma*sqrt(process.variability)/2)) 
}
