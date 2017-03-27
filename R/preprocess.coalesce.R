preprocess.coalesce <- function(
  x #Matrix or Data Frame
  ,left.to.right = T
) {
  ret <- rep(NA, nrow(x))
  
  iter <- ifelse(left.to.right, 1:ncol(x), ncol(x):1)
  
  for (i in iter) {
    if (is.factor(x[,i])) {
      x[,i] <- as.character(x[,i])
    }
    
    idx.na <- which(is.na(ret))
    
    if (length(idx.na) > 0) {
      ret[idx.na] <- x[idx.na,i]
    }
  }
  
  ret
}