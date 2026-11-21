summary.lolcat.msa.nominal.concordance <- function(object, ...) {
  ret <- list()
  
  comp.operators <- object$comp.operators
  comp.standard <- object$comp.standard
  conf.level <- object$conf.level
  mode <- NA
  
  if (length(comp.operators) > 0) {
    o <- list()
    
    o$measurements <- list()
    o$conf.level <- conf.level
    o$mode <- mode
    o$comparisons <- comp.operators
    
    class(o) <- "lolcat.msa.nominal.internalconsistency"  
    
    ret$BetweenOperators <- summary(o)
    
  }
  
  if (length(comp.standard) > 0) {
    o <- list()
    
    o$measurements <- list()
    o$conf.level <- conf.level
    o$mode <- mode
    o$comparisons <- comp.standard
    
    class(o) <- "lolcat.msa.nominal.internalconsistency"  
    
    ret$WithStandard <- summary(o)
  }
  
  ret
    
}