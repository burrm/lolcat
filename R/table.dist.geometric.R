table.dist.geometric <- function(p
                                 ,max.x = ceiling(4*(1-p)/p^2)) {
  
  d              <- data.frame(x=0:max.x)
  d$p.at.x       <- dgeom(0:max.x, p = p)
  d$eq.and.above <- pgeom(0:max.x, p = p, lower.tail = F) + d$p.at.x
  d$eq.and.below <- pgeom(0:max.x, p = p)
  
  rownames(d) <- d$x
  
  d
}