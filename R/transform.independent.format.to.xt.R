transform.independent.format.to.xt <-function(x_row
                                              ,x_col
                                              ,weight = rep(1,length(x_row)) ) {
  unique_x_row <- unique(x_row)
  unique_x_col <- unique(x_col)
  
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
