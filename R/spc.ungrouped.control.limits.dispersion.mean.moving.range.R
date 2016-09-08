spc.ungrouped.control.limits.dispersion.mean.moving.range <- function(
  standard.error 
  ,sample.size=2
) {
  
  list(UCL = standard.error*spc.constant.calculation.D4(sample.size = sample.size) 
       ,center.line = standard.error
       ,LCL = standard.error*spc.constant.calculation.D3(sample.size=sample.size))
  
}