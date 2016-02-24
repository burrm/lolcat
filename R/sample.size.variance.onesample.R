sample.size.variance.onesample <- function(
  var.est = 2
  ,h0.var = 1
  ,alpha = .05
  ,beta = .1
  ,alternative = c("two-sided","greater") #"less" won't work well
  ,details = TRUE
  ,power.from.actual = F #report parameter power instead of true power
  
  
) {

  ratio <- var.est/h0.var
  n <- 2  

  chi.fn <- if (alternative[1] == "two-sided") {
    function(n) {qchisq(alpha/2, n-1, lower.tail = FALSE)}
  } else if (alternative[1] == "greater") {
    function(n) {qchisq(alpha, n-1, lower.tail = FALSE)}
  } else {
    function(n) {qchisq(alpha, n-1, lower.tail = TRUE)}
  }
  
  beta.fn <- if (alternative[1] == "two-sided" | alternative[1] == "greater") {
    function(chi, ratio, n) {
      pchisq(chi/ratio, n-1)
    }
  } else {
    function(chi, ratio, n) {
      pchisq(chi/ratio, n-1, lower.tail = FALSE)
    }
  }
  
  

  current.beta <- beta.fn(chi.fn(n),ratio,n)
  
  while (current.beta >= beta) {
    n <- n+1
  
    current.beta <- beta.fn(chi.fn(n),ratio,n)
  }
  

  if (power.from.actual) {
    
  } else {
    
    beta <- current.beta
  }
  
  if (details) {
    as.data.frame(list(test="chi-square"
                       ,type = "one.sample"
                       ,alternative = alternative[1]
                       ,sample.size = n
                       ,df = n - 1
                       ,ratio = ratio
                       ,alpha = alpha
                       ,conf.level = 1-alpha
                       ,beta = beta
                       ,power = 1- beta
    ))
    
  }
  else {
    n
  }
    
}

#sample.size.variance.onesample()

