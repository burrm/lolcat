frequency.dist.ungrouped <- function(x, na.rm = T) {
  tbl.x <- table(x, useNA = ifelse(na.rm, "no", "ifany"))
  
  d <- data.frame(value = names(tbl.x), count = rmnames(as.vector(tbl.x)))
  names(d) <- c("value", "freq")
  
  if (is.factor(d$value)) {
    d$value <- as.numeric(levels(d$value)[d$value])
  } else if (is.character(d$value)) {
    d$value <- as.numeric(d$value)
  }
  
  if (nrow(d) > 1) {
    total.n <- sum(d$freq)
  
    d$rel.freq <- d$freq/ total.n
    d$cum.up <- cumsum(d$rel.freq)
    d$cum.down <- c(1, (1-d$cum.up)[1:(nrow(d)-1)])
  } else {
    d$rel.freq <- 1
    d$cum.up <- 1
    d$cum.down <- 1
  }
  
  
  d  
}