spc.ungrouped.control.limits.dispersion.mean.moving.range <- function(
  standard.error 
  ,sample.size=2
  ,n.sigma = 3
) {
  
  list(UCL = standard.error*spc.constant.calculation.D4(sample.size = sample.size, n.sigma = n.sigma) 
       ,center.line = standard.error
       ,LCL = standard.error*spc.constant.calculation.D3(sample.size=sample.size, n.sigma = n.sigma))
  
}