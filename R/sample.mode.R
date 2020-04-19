sample.mode <- function(x) {
  dist <- frequency.dist.ungrouped(x)
  m <- dist$value[which(dist$freq == max(dist$freq))]
  if (nrow(dist) > 1 & length(m) == nrow(dist)) {
    m <- NA
  }

  m
}