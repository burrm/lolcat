spc.ungrouped.control.limits.mean.moving.range <- function(
   center.line 
  ,standard.error 
  ,n.sigma = 3
  ,E.const = 2.6587
) {
  add_to_ret <- n.sigma * E.const * standard.error / 3
  
  list(UCL = center.line + add_to_ret 
       ,center.line = center.line
       ,LCL = center.line - add_to_ret)
  
}