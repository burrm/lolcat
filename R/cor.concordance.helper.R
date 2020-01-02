cor.concordance.helper <- function(x1, x2, call.rank = T) {
  ret <- list()
  
  d <- data.frame(x1 = x1, x2 = x2)
  d <- na.omit(d)
  
  x1 <- d$x1
  x2 <- d$x2
  
  if (call.rank) {
    rank.x1 <- rank(x1)
    rank.x2 <- rank(x2)
  } else {
    rank.x1 <- x1
    rank.x2 <- x2
  }
  
  #Order everything by order(rank.x1)
  ord.x1 <- order(rank.x1)
  
  pair <- (1:length(x1))[ord.x1]
  x1 <- x1[ord.x1]
  x2 <- x2[ord.x1]
  rank.x1 <- rank.x1[ord.x1]
  rank.x2 <- rank.x2[ord.x1]
  
  ret$pair <- pair
  ret$x1 <- x1
  ret$x2 <- x2
  ret$rank.x1 <- rank.x1
  ret$rank.x2 <- rank.x2
  
  cd.matrix <- matrix("N/A", nrow = length(x1), ncol = length(x1))
  colnames(cd.matrix) <- pair
  rownames(cd.matrix) <- pair

    #Determine concordant/discordant pairs
  
  for (i in 1:(ncol(cd.matrix)-1)) {
    for (j in (i+1):ncol(cd.matrix)) {
      x_i <- rank.x1[i]
      x_j <- rank.x1[j]
      
      y_i <- rank.x2[i]
      y_j <- rank.x2[j]
      
      cd.matrix[i,j] <- if ((x_i > x_j && y_i > y_j) || (x_i < x_j && y_i < y_j) ) {
        "C"
      } else if ((x_i > x_j && y_i < y_j) || (x_i < x_j && y_i > y_j)) {
        "D"
      } else {
        "T"
      }
    }
  }
  
  ret$cd.matrix <- cd.matrix

  ret$count.concordant <- length(which(cd.matrix == "C"))
  ret$count.discordant <- length(which(cd.matrix == "D"))
  ret$count.tie <- length(which(cd.matrix == "T"))
  
  t.x1 <- table(rank.x1)
  ret$ties.x1 <- as.vector(t.x1)
  names(ret$ties.x1) <- names(t.x1)
  
  t.x2 <- table(rank.x2)
  ret$ties.x2 <- as.vector(t.x2)
  names(ret$ties.x2) <- names(t.x2)
  
  
  ret  
}

