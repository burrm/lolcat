#   data <- expand.grid(group = c("a","b")
#                       ,specimen = 1:25
#                       )
#   data$cm <- c(rnorm(25,2,3),rnorm(25,0,1))
#  
#  zz <- t.test.twosample.independent.fx(cm~group, data=data)
# zz
 
t.test.twosample.independent.fx <- function(fx #cm ~ group
                                            ,data
                                            ,...) {
  summary.out <- summary.impl(fx, data = data, stat.n = T, stat.mean =T, stat.var = T, format.generate.cellcodes = T)

  if (nrow(summary.out) == 2) {
    t.test.twosample.independent.simple(
      sample.mean.g1 = summary.out$mean[1]
      ,sample.variance.g1 = summary.out$var[1]
      ,sample.size.g1 = summary.out$n[1]
      ,sample.mean.g2 = summary.out$mean[2]
      ,sample.variance.g2 = summary.out$var[2]
      ,sample.size.g2 = summary.out$n[2]
      ,...
    )
  } else if (nrow(summary.out) > 2) {
    cmbn <- combn(summary.out$cell.code, 2)

    ret <- list()
    
    for (i in 1:ncol(cmbn)) {
      g1 <- cmbn[1,i]
      g2 <- cmbn[2,i]
      
      idx_1 <- which(summary.out$cell.code == g1)
      idx_2 <- which(summary.out$cell.code == g2)
      
      ret[[paste(g1,"vs.",g2)]] <- t.test.twosample.independent.simple(
        sample.mean.g1 = summary.out$mean[idx_1]
        ,sample.variance.g1 = summary.out$var[idx_1]
        ,sample.size.g1 = summary.out$n[idx_1]
        ,sample.mean.g2 = summary.out$mean[idx_2]
        ,sample.variance.g2 = summary.out$var[idx_2]
        ,sample.size.g2 = summary.out$n[idx_2]
        ,...
      )
      
      ret[[paste(g1,"vs.",g2)]]$data.name <- paste(g1,"vs.",g2)
    }
    
    ret
    
  } else {
    stop("Need to provide at least 2 groups.")
  }
}