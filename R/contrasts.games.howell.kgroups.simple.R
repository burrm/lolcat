contrasts.games.howell.kgroups.simple <- function(
  group.label = 1:length(group.mean)   # Group Labels - character string recommended
  ,group.mean = c(1,2,3)               # vector with k group means 
  ,group.variance = c(1,4,9)           # vector with k group variances
  ,group.sample.size = c(10,10,10)     # vector with k group sample sizes
  ,conf.level.familywise = .95         # 1 - Familywise type 1 error rate 
  ,n.means = length(group.mean)        # Number of means being compared
  ,mean.squared.error = NA # If not NA, use Kirk equation, if NA, use PHAST-TM equation
  ,alternative = c("greater")#, "two.sided", "less")
) {

  data.in <- data.frame(label        = group.label
                        ,mean        = group.mean
                        ,variance    = group.variance
                        ,sample.size = group.sample.size)
  
  data.in <- data.in[order(data.in$mean, data.in$variance),]

  list.tests      <- list()
  matrix.decision <- matrix("", nrow = nrow(data.in), ncol = nrow(data.in))
  matrix.p.value  <- matrix(1,  nrow=nrow(data.in),   ncol = nrow(data.in))
  matrix.ref      <- matrix("", nrow=nrow(data.in),   ncol = nrow(data.in))
  
  rownames(matrix.decision) <- data.in$label 
  rownames(matrix.ref) <- data.in$label
  rownames(matrix.p.value) <- data.in$label
  
  colnames(matrix.decision) <- data.in$label 
  colnames(matrix.ref) <- data.in$label
  colnames(matrix.p.value) <- data.in$label
  
  p.value <- 1-conf.level.familywise
  
  for (i in 1:(nrow(data.in)-1)) {
    for (j in (i+1):nrow(data.in)) {
      label_i <- data.in$label[i]
      label_j <- data.in$label[j]
      
      list.label <- paste(label_i, "vs.", label_j)
      
      test.result <- contrasts.games.howell.twogroups.simple(
        weight                 = c(-1,1)                
        ,group.mean            = c(data.in$mean[i],data.in$mean[j]) 
        ,group.variance        = c(data.in$variance[i],data.in$variance[j])      
        ,group.sample.size     = c(data.in$sample.size[i],data.in$sample.size[j]) 
        ,conf.level.familywise = conf.level.familywise     
        ,n.means               = n.means  
        ,mean.squared.error    = mean.squared.error
        ,alternative           = alternative
      )
      
      list.tests[[list.label]] <- test.result
      
      matrix.p.value[i,j] <- test.result$p.value
      matrix.decision[i,j] <- ifelse(test.result$p.value < p.value, "Reject", "")
      matrix.ref[i,j] <- list.label
      
      matrix.p.value[j,i] <- test.result$p.value
      matrix.decision[j,i] <- ifelse(test.result$p.value < p.value, "Reject", "")
      matrix.ref[j,i] <- list.label
      
    }
  }

  
  retval <- list(
       data = data.in
       ,list.tests = list.tests
       ,matrix.p.value = matrix.p.value
       ,matrix.decision = matrix.decision
       ,matrix.ref = matrix.ref
       )
  
  retval
}
