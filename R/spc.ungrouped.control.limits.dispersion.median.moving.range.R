spc.ungrouped.control.limits.dispersion.median.moving.range <- function(
  standard.error 
  ,sample.size=2
  ,n.sigma = 3
) {
  
  list(UCL = standard.error*spc.constant.calculation.D6(sample.size = sample.size, n.sigma = n.sigma) 
       ,center.line = standard.error
       ,LCL = standard.error*spc.constant.calculation.D5(sample.size=sample.size, n.sigma = n.sigma))
  
}

#spc.ungrouped.control.limits.dispersion.median.moving.range(2)

#spc.constant.calculation.D5(2)
#spc.constant.calculation.D6(2)

