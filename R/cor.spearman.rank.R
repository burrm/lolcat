#' @rdname cor.spearman.rank.simple
cor.spearman.rank <- function (x1
                               ,x2
                               ,conf.level = .95
                               ,alternative = c("two.sided","less","greater")) {
  validate.htest.alternative(alternative = alternative)
  
  d <- data.frame(x1 = x1
                  ,x2 = x2)
  
  d <- na.omit(d)
  
  r_sp <- cor(d$x1,d$x2,method="spearman")
  n <- nrow(d)
  
  cor.spearman.rank.simple(r_sp = r_sp
                           ,sample.size = n
                           ,conf.level = conf.level
                           ,alternative = alternative)
    
}