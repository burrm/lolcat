sample.mode <- function(x) {
  m <- NA

  if (length(x) > 1) {
  
    if (is.character(x[1])) {
      m.out <- sample.mode(factor(x))
    
      if (!is.na(m.out[1])) {
        m <- levels(m.out)[m.out]
      }
    } else if (is.factor(x[1])) {
      m.out <- lolcat::sample.mode(as.numeric(x))
      if (!is.na(m.out[1])) {
        m <- factor(levels(x)[m.out], levels(x))
      } else {
        m <- factor(NA, levels(x))
      }
    } else {
      dist <- frequency.dist.ungrouped(x)
      m <- dist$value[which(dist$freq == max(dist$freq))]
      if (nrow(dist) > 1 & length(m) == nrow(dist)) {
        m <- NA
      }    
    }
  } else if (length(x) == 1) {
    m <- x
  }
  
  m
}