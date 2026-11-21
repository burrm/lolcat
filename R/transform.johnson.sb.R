#ln((x-epsilon)/(lambda+epsilon-x))

transform.johnson.sb <- function(x
                                 ,gamma = 0
                                 ,eta = 1
                                 ,lambda = 1
                                 ,epsilon = 0
                                 ,inverse = F
) {
  
  if (!inverse) {
    gamma + eta*log((x-epsilon)/(lambda+epsilon-x))    
  } else {
    e1 <- exp((-gamma+x)/eta)
    (lambda*e1+epsilon+epsilon*e1)/(1+e1)    

    
    #(   (lambda + epsilon) * exp(x - gamma/eta) + epsilon  )/(1+ exp(x - gamma/eta)) 
    #( epsilon*(1+ exp(x - gamma/eta)) +  lambda  * exp(x - gamma/eta)  )/(1+ exp(x - gamma/eta)) 
  }
  
  
}