transform.xt.to.independent.format <- function(x, row.var="row", col.var = "col", use.weight = F) {
  if (is.null(rownames(x))) {
    rownames(x) <- 1:nrow(x)
  }  
  
  if (is.null(colnames(x))) {
    colnames(x) <- 1:ncol(x)
  }
  
  x1 <- NULL
  x2 <- NULL
  weight <- integer(0)
  
  for (i in 1:ncol(x)) {
    for (j in 1:nrow(x)) {
      i.name <- colnames(x)[i]
      j.name <- rownames(x)[j]
      ct <- x[j,i]
      
      if (use.weight) {
        x1 <- c(x1, j.name)
        x2 <- c(x2, i.name)
        weight <- c(weight, ct)
        
      } else {
        x1 <- c(x1, rep(j.name, ct))
        x2 <- c(x2, rep(i.name, ct))
      }
    }
  }
  
  x1 <- factor(x1)
  x2 <- factor(x2)
  
  
  ret<- list()
  ret[[row.var]] <- x1
  ret[[col.var]] <- x2
  
  if (use.weight) {
    ret[["weight"]] <- weight
  }
  
  ret
}


