table.dist.binomial <- function(n = 10
                                ,p = .5) {
  
  d              <- data.frame(x=0:n)
  d$p.at.x       <- dbinom(0:n, size = n, prob = p)
  d$eq.and.above <- pbinom(0:n, size=n, prob = p, lower.tail = F) + d$p.at.x
  d$eq.and.below <- pbinom(0:n, size=n, prob = p)
  
  d
}