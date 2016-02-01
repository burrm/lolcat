spc.control.limits.dispersion.mean.moving.range <- function(
  standard.error 
  ,n=2
) {
  
  list(UCL = standard.error*spc.constant.calculation.D4(n=n) 
       ,center.line = standard.error
       ,LCL = standard.error*spc.constant.calculation.D3(n=n))
  
}