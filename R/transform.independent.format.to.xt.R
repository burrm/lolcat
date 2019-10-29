transform.independent.format.to.xt <-function(x_row
                                              ,x_col
                                              ,weight = rep(1,length(x_row)) 
                                              ,na.rm = T
                                              ,sort.names = T
                                              ,x_row_name = "row_label"
                                              ,x_col_name = "col_label"
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

  if (x_row_name == x_col_name) {
    x_col_name <- paste0(x_col_name, ".1")
  }
    
  newnames <- list()
  newnames[[x_row_name]] <- unique_x_row
  newnames[[x_col_name]] <- unique_x_col
  
  dimnames(ret) <- newnames
  
  for (i in 1:length(unique_x_row)) {
    for (j in 1:length(unique_x_col)) {
      idx <- which(x_row == unique_x_row[i] & x_col == unique_x_col[j])
      
      ret[i,j] <- sum(weight[idx])
    }
  }

  ret
}
