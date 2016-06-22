#ln((x-epsilon)/lambda)

transform.johnson.sl <- function(x
                                 ,gamma = 0
                                 ,eta = 1
                                 ,lambda = 1
                                 ,epsilon = 0
                                 ,inverse = F
) {
  if (!inverse) {
    gamma + eta*log((x-epsilon)/lambda)    
  } else {
    #exp(-gamma/eta)*( exp(gamma/eta +1) + lambda* exp(x / eta) )
    epsilon + lambda * exp((x-gamma)/eta)
    
  }

}