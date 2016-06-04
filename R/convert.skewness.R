convert.skewness <- function(x
                             ,sample.size
                             ,from = c("fisher", "pearson")
                             ,to   = c("pearson", "fisher")
) {
  ret <- x
  n <- sample.size
  
  if (from[1] == "fisher" && to[1] == "pearson") {
    root.b1 <- x * ( (n-2)/sqrt(n*(n-1)) )
    ret <- root.b1
    
  } else if (from[1] == "pearson" && to[1] == "fisher") {
    g1 <- x / ( (n-2)/sqrt(n*(n-1)) )
    ret <- g1
  }
  
  ret
}