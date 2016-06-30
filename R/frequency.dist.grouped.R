frequency.dist.grouped <- function(
                       x
                       ,interval.size = NA
                       ,width.consider = lolcat.default.width.consider 
                       ,na.rm = T
                       ,clean.leading.zeroes = T
                       ,clean.trailing.zeroes = T
) {
  x <- na.omit(x)
  x <- x[is.finite(x)]
  
  if (is.na(interval.size)) {
    r <- (max(x)-min(x))/10
    w.r <- abs(width.consider - r)
    interval.size <- width.consider[which(w.r == min(w.r))][1]
  }
  
  x.min <- floor(min(x)) 
  while (x.min %% interval.size != 0) {
    x.min <- x.min - min(1, interval.size)
  }
  
  bins <- seq(x.min, max(x)+interval.size+1, interval.size)
  
  x.cut <- cut(x, bins, right = F)
  
  tbl.x <- table(x.cut, useNA = ifelse(na.rm, "no", "ifany"))
  
  d <- data.frame(count = rmnames(tbl.x))
  names(d) <- c("value", "freq")
  
  labels <- as.character(d$value)
  
  labels.split <- strsplit(labels, ",")
  
  d$min <- unlist(lapply(labels.split, FUN = function(x) {
    as.numeric(sub("[","", x[1],fixed = T))
  }))
  
  d$max <- unlist(lapply(labels.split, FUN = function(x) {
    as.numeric(sub(")","", x[2],fixed = T))
  }))

  d <- data.frame(l = rep("[", nrow(d)) 
                  ,min = d$min
                  ,midpoint = (d$min + d$max)/2
                  ,max = d$max
                  ,u = rep(")", nrow(d))
                  ,freq = d$freq
                  )

  total.n <- sum(d$freq)
  
  d$rel.freq <- d$freq/ total.n
  d$cum.up <- cumsum(d$rel.freq)
  d$cum.down <- c(1, (1-d$cum.up)[1:(nrow(d)-1)])
  
  if (clean.leading.zeroes) {
    zz.rle <- rle(d$freq == 0)
    
    if (zz.rle$values[1]) {
      d <- d[(zz.rle$lengths[1]+1):nrow(d),]
    }
  }
  
  
  if (clean.trailing.zeroes) {
    zz.rle <- rle(d$freq == 0)
    
    last_idx <- length(zz.rle$lengths)
    
    if (zz.rle$values[last_idx]) {
      d <- d[1:(nrow(d) - zz.rle$lengths[last_idx]),]
    }
  }

  rownames(d) <- 1:nrow(d)
    
  d  
}