table.dist.hypergeometric <- function( 
  pop.success.count
  ,total.count
  ,sample.size
) {
  
  m <- pop.success.count
  n <- total.count - m
  
  d              <- data.frame(x=0:sample.size)
  d$p.at.x       <- dhyper(0:sample.size, m = m, n = n, k = sample.size)
  d$eq.and.above <- phyper(0:sample.size, m = m, n = n, k = sample.size, lower.tail = F) + d$p.at.x
  d$eq.and.below <- phyper(0:sample.size, m = m, n = n, k = sample.size)
  
  rownames(d) <- d$x
  names(d)[1] <- "success.count"
  
  d
}
