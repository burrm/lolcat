#Kirk pg. 176
#Constraint - length of input vectors should be equal, otherwise assumed that 
#             user knows what they are doing...
contrasts.df.welch.simple <- function(
  weight = c(1,-1),              # vector with 2 or more contrast coefficients
  #group.mean = c(1,-1),         # vector with 2 or more group means 
  group.variance = c(1,1),       # vector with 2 or more group variances
  group.sample.size = c(10,10)   # vector with 2 or more group sample sizes
) {
  sum(weight^2 * group.variance / group.sample.size)^2 / sum((weight^4 * group.variance^2)/(group.sample.size^2 * (group.sample.size-1)) )
  
}