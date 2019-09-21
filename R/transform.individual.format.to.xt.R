transform.individual.format.to.xt <-function(x_row
                                              ,x_col
                                              ,weight = rep(1,length(x_row)) 
                                              ,na.rm = T
                                              ,sort.names = T
) {
  transform.independent.format.to.xt(x_row = x_row
                                     ,x_col = x_col
                                     ,weight = weight 
                                     ,na.rm = na.rm
                                     ,sort.names = sort.names)
}