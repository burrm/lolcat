mlr.mallow.cp.simple <- function(r.squared.model
                                 ,parameter.count.model
                                 ,r.squared.all.vars
                                 ,parameter.count.all.vars
                                 ,sample.size) {
  (1-r.squared.model)*(sample.size - parameter.count.all.vars)/(1-r.squared.all.vars) - (sample.size -2*(parameter.count.model + 1))
  
}