#' Perform Study of Nominal Scale Rater's Internal Consistency
#' 
#' Evaluate two or more measurements' agreement for a set of subjects, usually within a single rater.
#'
#' @param ... Vectors - character or factor - One or more rater's evaluations of subjects
#' @param conf.level Confidence level to use for the statistical tests
#'
#' @return A data structure including details of the tests between measurements. Use summary() to summarize results. 
msa.nominal.internalconsistency <- function(
  ... #2 or more nominal measurements of k items
  ,conf.level = .95
) {
  ret <- list()
  comp <- list()

  measurements <- list(...)
  
  k <- length(measurements)
  measurement.names <- 1:k
  
  if (!is.null(names(measurements))) {
    measurement.names <- names(measurements)
  } else {
    measurement.names <- sapply(substitute(list(...)), deparse)[-1]
  }
  
  for (i in 1:k) {
    for (j in i:k) {
      if (i != j) {
        this.name <- paste0(measurement.names[i]," v ",measurement.names[j])
        
        comp[[this.name]] <- msa.nominal.concordance.simple(
          measurement1 = measurements[[i]], 
          measurement2 = measurements[[j]], 
          conf.level = conf.level)
      }
    }
  }

  df <- do.call(cbind.data.frame, measurements)
  
  ret$measurements <- measurements
  ret$conf.level <- conf.level
  ret$mode <- apply(df, 1, sample.mode)
  ret$comparisons <- comp

  class(ret) <- "lolcat.msa.nominal.internalconsistency"
  
  ret
}