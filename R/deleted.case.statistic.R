deleted.case.statistic <- function(x, FUN = mean) {
  ret <- x
  
  for (i in 1:length(x)) {
    this_x <- x[-i]
    ret[i] <- FUN(this_x)
  }
  
  ret
}