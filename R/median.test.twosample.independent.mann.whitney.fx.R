median.test.twosample.independent.mann.whitney.fx <- function(
  fx
  ,data
  ,...
) {
  cell <- compute.group.cell.codes(fx, data = data)
  
  fx.terms<-terms(fx)
  response<-all.vars(fx)[attributes(fx.terms)$response]
  
  response.split <- split(data[[response]], cell)
  
  if (length(response.split) == 2) {
    median.test.twosample.independent.mann.whitney(
      g1 = response.split[[1]]
      ,g2 = response.split[[2]]
      ,...
    )
  } else if (length(response.split) > 2) {
    cmbn <- combn(1:length(response.split), 2)
    
    ret <- list()
    
    for (i in 1:ncol(cmbn)) {
      g1 <- cmbn[1,i]
      g2 <- cmbn[2,i]
      
      ret[[paste(g1,"vs.",g2)]] <- median.test.twosample.independent.mann.whitney(
        g1 = response.split[[g1]]
        ,g2 = response.split[[g2]]
        ,...
      )
      
      ret[[paste(g1,"vs.",g2)]]$data.name <- paste(g1,"vs.",g2)
    }
    
    ret
    
  } else {
    stop("Need to provide at least 2 groups.")
  }
}