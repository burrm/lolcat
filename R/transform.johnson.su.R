#arcsinh((x-epsilon)/lambda)

transform.johnson.su <- function(x
                                 ,gamma = 0
                                 ,eta = 1
                                 ,lambda = 1
                                 ,epsilon = 0
                                 ,inverse = F
) {
  
  if (!inverse) {
    gamma + eta*asinh((x-epsilon)/lambda)    
  } else {
    lambda*sinh((x-gamma)/eta)+epsilon
  }
  

}