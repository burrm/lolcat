convert.kurtosis <- function(x
                             ,sample.size
                             ,from = c("fisher", "pearson")
                             ,to   = c("pearson", "fisher")
) {
  ret <- x
  n <- sample.size
  
  if (from[1] == "fisher" && to[1] == "pearson") {
    b2 <- ( (n-2)*(n-3) / ((n+1)*(n-1))  ) * x + 3*(n-1)/(n+1)
    ret <- b2
    
  } else if (from[1] == "pearson" && to[1] == "fisher") {
    g2 <- (x - 3*(n-1)/(n+1)) / ( (n-2)*(n-3) / ((n+1)*(n-1))  ) 
    ret <- g2
  }
  
  ret
}