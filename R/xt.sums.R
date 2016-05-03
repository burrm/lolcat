xt.sums <- function(x) {
  x <- cbind(x, apply(x,1,FUN = sum))
  x <- rbind(x, apply(x,2,FUN = sum))
  
  rownames(x)[nrow(x)]<-"col.sum"
  colnames(x)[ncol(x)]<-"row.sum"
  
  x
}