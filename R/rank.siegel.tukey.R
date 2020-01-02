rank.siegel.tukey <- function(g1
                              ,g2
                              ,details = T
                              ,ties.method = c("average", "max", "min", "none", "first", "random") #TODO: ,
                              ) {
  
  ties.method<- ties.method[1]
  
  d <- data.frame(x=c(g1,g2))
  d$grp <- c(rep(1,length(g1)),rep(2,length(g2)))  
  d$idx <- c(1:length(g1),1:length(g2))
  
  d <- d[order(d$x),]
  
  i <- 0
  i1 <- 1
  i2 <- nrow(d)

  d$ranks <- rep(NA, nrow(d))

  while(is.na(d$ranks[i1])) {
    i <- i+1
    
    d$ranks[i1] <- i
    i1 <- i1+1
    
    if (is.na(d$ranks[i2])) {
      i <- i+1

      d$ranks[i2] <- i
      i2 <- i2-1
    }
    
    if (is.na(d$ranks[i2])) {
      i <- i+1
      
      d$ranks[i2] <- i
      i2 <- i2-1
    }
    
    if (is.na(d$ranks[i1])) {
      i <- i+1
      
      d$ranks[i1] <- i
      i1 <- i1+1
    }
  }
  
  d$ranks.final <- d$ranks
  d <- d[order(d$grp,d$idx),]
  
  # Tie adjustment
  
  if (ties.method != "none") {
    tbl.x <- unique(d$x)
    
    for (i in tbl.x) {
      idx.x <- which(d$x == i)
      vals <- d$ranks[idx.x]
      
      d$ranks.final[idx.x] <- if (ties.method == "average") {
        mean(vals)
      } else if (ties.method == "max") {
        max(vals)
      } else if (ties.method == "min") {
        min(vals)
      } else if (ties.method == "first") {
        vals[1]
      } else if (ties.method == "random") {
        if (length(vals) > 1) {
          sample(vals,size = length(vals), replace = F)
        } else {
          vals
        }
      }
    }
  
  }

  ret <- d
  
  if (details) {
    
  } else {
    ret <- split(d$ranks.final, f = d$grp)
    names(ret) <- c("g1", "g2")
  }
  
  ret
}