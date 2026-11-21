#' Normalizing Transformations - Box-Cox/Ladder of Powers - Parameter Recommendation 
#' 
#' Recommend parameters to optimally transform sample data to normality based on the Box Cox transformation.
#' 
#' @param x Vector/numeric - sample data.
#' @param lambda.min Scalar/numeric - Minimum exponent.
#' @param lambda.max Scalar/numeric - Maximum exponent.
#' @param step - Scalar/numeric - The step size between min and max exponent to try.
#' @param correct.min Scalar/logical - Shift data by minimum in sample to avoid transformation/reverse transformation issues.
#' @param target - Scalar/character - The target distribution.
#' @param metric - Scalar/character - Which metric to use for evaluating transformations.
#'
#' @return Data frame with best parameters found in search. 
recommend.box.cox <- function(
  x
  ,lambda.min = -5
  ,lambda.max = 5
  ,step = .1
                            
  #Shift data for possible exponent issues
  ,correct.min = T
  ,target = c("normal")
  ,metric = c("skewness+kurtosis", "skewness", "kurtosis")
                            
) {
  box.cox.out <- explore.box.cox(x
                                 ,lambda.min =lambda.min
                                 ,lambda.max =lambda.max
                                 ,step = step
                                 ,correct.min = correct.min
                                 )

  box.cox.out$sum.g3.g4 <- if (metric[1] == "skewness+kurtosis") {
    abs(box.cox.out$g3.skewness) + abs(box.cox.out$g4.kurtosis)  
  } else if (metric[1] == "skewness") {
    abs(box.cox.out$g3.skewness)
  } else if (metric[1] == "kurtosis") {
    abs(box.cox.out$g4.kurtosis)
  }
  
  min.g3.g4 <- min(box.cox.out$sum.g3.g4)
  
  row <- which(box.cox.out$sum.g3.g4 == min.g3.g4)
  
  data.frame(lambda = box.cox.out$lambda[row[1]]
             ,correct.min = box.cox.out$correct.min[row[1]]
             ,g3.skewness = box.cox.out$g3.skewness[row[1]]
             ,g3test.p = box.cox.out$g3test.p[row[1]]
             ,g4.kurtosis = box.cox.out$g4.kurtosis[row[1]]
             ,g4test.p = box.cox.out$g4test.p[row[1]]
             
             )
}