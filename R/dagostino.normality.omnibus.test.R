#' @rdname dagostino.normality.omnibus.test.simple
dagostino.normality.omnibus.test <- function(x
                                             ,conf.level = .95
                                             ,alternative = #c("two.sided") #,"less",
                                               "greater"#)
                                             
)
{
  validate.htest.alternative(alternative = alternative)
  x <- na.omit(x)

  fisher.g1   <- skewness(x)
  fisher.g2   <- kurtosis(x)
  sample.size <- length(x)
    
  dagostino.normality.omnibus.test.simple(skewness = fisher.g1
                                          , kurtosis = fisher.g2
                                          , sample.size = sample.size
                                          , conf.level = conf.level)
}

#dagostino.normality.omnibus.test(Framingham)
#convert.skewness(1.049102293, 62)
#convert.kurtosis(1.815791319, 62)
