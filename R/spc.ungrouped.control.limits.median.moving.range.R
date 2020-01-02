
spc.ungrouped.control.limits.median.moving.range <- function(
  center.line 
  ,standard.error 
  ,n.sigma = 3
) {
  add_to_ret <- n.sigma * 3.145 * standard.error / 3
  
  list(UCL = center.line + add_to_ret 
       ,center.line = center.line
       ,LCL = center.line - add_to_ret)
  
}


#spc.ungrouped.control.limits.mean.moving.range(1,.5)
#spc.ungrouped.control.limits.median.moving.range(1,.5/spc.constant.calculation.d4(2))

#spc.ungrouped.control.limits.median.moving.range(1,.5)

#spc.constant.calculation.D6(3)
