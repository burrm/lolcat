cor.goodman.kruskal.gamma.onesample <- function(
  x #ordered contingency table (row and column)
  ,null.hypothesis.gamma = 0
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  #TODO: Make this more efficient someday
  x.nc <- matrix(0, nrow= nrow(x), ncol = ncol(x))
  x.nd <- matrix(0, nrow= nrow(x), ncol = ncol(x))
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  #n_c - below and to the right
  for (i in 1:(nr-1)) {
    for (j in 1:(nc-1)) {
      x.nc[i,j] <- x[i,j]*sum(x[(i+1):nr,(j+1):nc])
    }
  }

  #n_d - below and to the left
  for (i in 1:(nr-1)) {
    for (j in 2:(nc)) {
      x.nd[i,j] <- x[i,j]*sum(x[(i+1):nr,1:(j-1)])
    }
  }
    
  n_c <- sum(x.nc)
  n_d <- sum(x.nd)
  
  G <- (n_c-n_d)/(n_c+n_d)
  N <- sum(x)
  
  
  se.est <- 1/sqrt((n_c+n_d)/(N*(1-G^2)))
  z <- (G-null.hypothesis.gamma)/se.est 
  
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  
  G.upper <- G + cv*se.est
  G.lower <- G - cv*se.est
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  retval<-list(data.name   = "ordered contingency table",
               statistic   = z, 
               estimate    = c(G = G 
                               ,se.est = se.est
                               ,total.subjects = N
                               ,count.concordant = n_c
                               ,count.discordant = n_d
               ),
               parameter   = null.hypothesis.gamma,
               p.value     = p.value,
               null.value  = null.hypothesis.gamma,
               alternative = alternative[1],
               method      = "Goodman and Kruskal's Gamma (G)",
               conf.int    = c(G.lower, G.upper)
  )
  
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "G"
  names(retval$parameter) <- "null hypothesis G"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}