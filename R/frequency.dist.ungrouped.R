frequency.dist.ungrouped <- function(x, na.rm = T) {
  tbl.x <- table(x, useNA = ifelse(na.rm, "no", "ifany"))
  
  d <- data.frame(count = rmnames(tbl.x))
  names(d) <- c("value", "freq")
  
  total.n <- sum(d$freq)
  
  d$rel.freq <- d$freq/ total.n
  d$cum.down <- cumsum(d$rel.freq)
  d$cum.up <- c(1, (1-d$cum.down)[1:(nrow(d)-1)])
  
  
  d  
}

?table
