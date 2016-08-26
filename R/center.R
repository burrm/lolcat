center <- function(x
                   ,center.function = mean
                   ,center.function.na.rm = T) {
  c1 <- center.function(
          ifelse(center.function.na.rm
                 ,na.omit(x) 
                 ,x) 
          )
  
  x - c1
  
}