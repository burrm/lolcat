#' Matrix - Row and Column Sums  
#' 
#' Calculate row sums, column sums, and total sum.
#'
#' @param x Matrix - matrix to calculate row sums, column sums, and total sum.
#'
#' @return x with additional row and column for marginal sums.
xt.sums <- function(x) {
  x <- cbind(x, apply(x,1,FUN = sum))
  x <- rbind(x, apply(x,2,FUN = sum))
  
  rownames(x)[nrow(x)]<-"col.sum"
  colnames(x)[ncol(x)]<-"row.sum"
  
  x
}