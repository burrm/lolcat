#' @rdname cor.kendall.tau.simple
cor.kendall.tau <- function(x1
                            ,x2
                            ,alternative = c("two.sided", "greater", "less")
                            ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  helper.out <- cor.concordance.helper(x1,x2)
  
  cor.kendall.tau.simple(
    count.concordant = helper.out$count.concordant
    ,count.discordant = helper.out$count.discordant
    ,ties.x1 = helper.out$ties.x1
    ,ties.x2 = helper.out$ties.x2
    ,sample.size = length(helper.out$x1)
    ,alternative = alternative
    ,conf.level = conf.level
  )
  
}