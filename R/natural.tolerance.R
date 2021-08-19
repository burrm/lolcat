natural.tolerance <- function(f #quantile function/closure
                                #interface is f(p, lower.tail)
                              ,total.area = 1-.9973
                              ,upper.tail = total.area / 2
                              ,lower.tail = total.area / 2
                              ,details = T
) {
  upper.limit <- f(p = upper.tail, lower.tail = F)
  lower.limit <- f(p = lower.tail, lower.tail = T)
  
  natural.tolerance = upper.limit - lower.limit
  
  if (!details) {
    natural.tolerance
  } else {
    data.frame(
      natural.tolerance = natural.tolerance
      ,lower.limit = lower.limit
      ,upper.limit = upper.limit
      ,lower.area = lower.tail
      ,upper.area = upper.tail 
     )
  }
  
}