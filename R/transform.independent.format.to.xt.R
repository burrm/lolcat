transform.independent.format.to.xt <-function(x_row
                                              ,x_col
                                              ,weight = rep(1,length(x_row)) 
                                              ,na.rm = T
                                              ,sort.names = T
                                              ) {
  if (na.rm) {
    d <- data.frame(x_row = x_row, x_col = x_col)
    d <- na.omit(d)
    x_row <- d$x_row
    x_col <- d$x_col
    
  }
  
  unique_x_row <- unique(x_row)
  unique_x_col <- unique(x_col)
  
  if (sort.names) {
    unique_x_row <- unique_x_row[order(unique_x_row)]  
    unique_x_col <- unique_x_col[order(unique_x_col)]  
  }
  
  ret <- matrix(0, nrow=length(unique_x_row), ncol=length(unique_x_col))
  rownames(ret) <- unique_x_row
  colnames(ret) <- unique_x_col
  
  for (i in 1:length(unique_x_row)) {
    for (j in 1:length(unique_x_col)) {
      idx <- which(x_row == unique_x_row[i] & x_col == unique_x_col[j])
      
      ret[i,j] <- sum(weight[idx])
    }
  }

  ret
}
