recommend.box.cox <- function(x
                            ,lambda.min = -5
                            ,lambda.max = 5
                            ,step = .1
                            
                            #Shift data for possible exponent issues
                            ,correct.min = T
                            ,target = c("normal")
                            
) {
  box.cox.out <- explore.box.cox(x
                                 ,lambda.min =lambda.min
                                 ,lambda.max =lambda.max
                                 ,step = step
                                 )

  box.cox.out$sum.g3.g4 <- abs(box.cox.out$g3.skewness) + abs(box.cox.out$g4.kurtosis)  
  min.g3.g4 <- min(box.cox.out$sum.g3.g4)
  
  row <- which(box.cox.out$sum.g3.g4 == min.g3.g4)
  
  list(lambda = box.cox.out$lambda[row[1]]
       ,correct.min = box.cox.out$correct.min[row[1]]
       )
}