table.dist.poisson <- function(lambda = 5
                               ,include.lambda = ifelse(
                                                 abs(1-lambda)<.3,
                                                 max(10,10/lambda),
                                                 max(5, 5/lambda))
) {
  n<-ceiling(include.lambda*lambda)
  
  d              <- data.frame(x=0:n)
  d$p.at.x       <- dpois(0:n, lambda = lambda)
  d$eq.and.above <- ppois(0:n, lambda = lambda, lower.tail = F) + d$p.at.x
  d$eq.and.below <- ppois(0:n, lambda = lambda)
  
  rownames(d) <- d$x
  
  d
  
  
}