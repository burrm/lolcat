#' Perform Study of Nominal Scale Agreement/Concordance With or Without Standard
#' 
#' Evaluate one or more rater's agreement for a set of subjects with or without a "standard" or "true" value.
#' This is sometimes called a nominal gage study or a study of interrater agreement. 
#' Note that all input vectors should be ordered by parts/subjects (i.e 1st value in rater 1's vector is first part, 
#' 2nd value in rater 2's vector is 2nd part, 3rd value in standard is 3rd part, etc.)
#'
#' @param ... Vectors - character or factor - One or more rater's evaluations of subjects
#' @param standard Vector - character or factor - the "standard" or "true" measurement
#' @param conf.level Confidence level to use for the statistical tests
#'
#' @return A data structure including all of the details of the tests between operators/standard. Use summary() to summarize results. 
msa.nominal.concordance <- function(
  ...            #1 or more measurements or results from msa.nominal.internalconsistency
  ,standard = NA #The standard measurement of each part
  ,conf.level = .95
) {
  ret <- list()
  comp.operators <- list()
  comp.standard <- list()
  
  operators <- list(...)
  
  k <- length(operators)
  operator.names <- 1:k
  
  if (!is.null(names(operators))) {
    operator.names <- names(operators)
  } else {
    operator.names <- sapply(substitute(list(...)), deparse)[-1]
  }
  
  # Operator agreement...
  
  for (i in 1:k) {
    for (j in i:k) {
      if (i != j) {
        this.name <- paste0(operator.names[i]," v ",operator.names[j])
        comp.operators[[this.name]] <- msa.nominal.concordance.simple(
          measurement1 = operators[[i]],
          measurement2 = operators[[j]],
          conf.level = conf.level
        )
      }
    }
  }
  
  # Operator agreement with standard...
  
  if (!is.na(standard[1])) {
    for (i in 1:k) {
      this.name <- paste0(operator.names[i]," v Standard")
      comp.standard[[this.name]] <- msa.nominal.concordance.simple(
        measurement1 = operators[[i]],
        standard,
        conf.level = conf.level
      )
    }
  }
  
  ret$operators <- operators
  ret$standard <- standard
  ret$conf.level <- conf.level
  ret$comp.operators <- comp.operators
  ret$comp.standard <- comp.standard
  
  class(ret) <- "lolcat.msa.nominal.concordance"
  
  ret
}