#' Friedman Two-Way Analysis of Variance By Ranks  
#' 
#' Perform Friedman's Analysis of Variance by Ranks.
#'
#' @param fx A formula defining groups and a dependent variable
#' @param data A data frame that corresponds to the formulas in fx.
#' @param order.by ordering vector for the data, does not need to be unique
#' @param tie.correct Tie correction (T/F) 
#' @param alternative Alternative hypothesis to be tested
#' @param conf.level Confidence level for test
#'
#' @return htest object containing results of the test. 
anova.dependent.friedman <- function(
  fx           #Grouping Function
  ,data = NULL #Data Frame
  ,order.by = 1:nrow(data) #Order By - does not need to be distinct
  ,tie.correct = T #Tie correction
  ,alternative = c("two.sided", "greater", "less")
  ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  data <- data[order(order.by),]  

  
  fx.terms<-terms(fx)
  
  response<-all.vars(fx)[attributes(fx.terms)$response]
  #iv.names<-attributes(terms(fx))$term.labels[which(attributes(fx.terms)$order == 1)]
  
  cell.codes <- compute.group.cell.codes(fx =fx, data = data)
  dv <- data[[response]]
  
  dv.split <- split(dv, cell.codes)
  
  dv.matrix <- do.call("cbind", dv.split)  #Organized by Group
  dv.matrix <- as.data.frame(t(dv.matrix)) #Subjects as columns
  
  dv.matrix <- do.call("cbind", lapply(dv.matrix, rank)) #Rank within subjects

  dv.matrix <- as.matrix(t(dv.matrix)) #Back to groups as columns

  common.sample.size <- nrow(dv.matrix)
  group.count <- ncol(dv.matrix)
  rank.sums <- apply(dv.matrix, 2, sum)

  chi.square <- (12/(common.sample.size*group.count*(group.count+1)))*sum(rank.sums^2) - 3*common.sample.size*(group.count+1)
  df <- group.count - 1
  
  C <- 1
  
  if (tie.correct) {
    tie.sum <- sum(apply(dv.matrix, 1, FUN = function(x) {
      zz <- as.vector(table(x))
      sum(sapply(zz, FUN = function(y) {y^3 - y}))
    }))
    
    tie.sum <- tie.sum/(common.sample.size*(group.count^3 - group.count))
    
    C <- C-tie.sum
    
    chi.square <- chi.square/C
  }
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pchisq(chi.square,df)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pchisq(chi.square,df,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pchisq(chi.square,df,lower.tail = TRUE)
  } else {
    NA
  }
  
  retval<-list(data.name   = "subject numbers, ranks within subject, and cell codes",
               statistic   = c(chi.square = chi.square), 
               estimate    = c(
                 df = df
                 ,common.sample.size = common.sample.size
                 ,number.of.groups = df+1
                 ,tie.correct.C = C
               ),
               parameter   = df,
               p.value     = p.value,
               null.value  = df,
               alternative = alternative[1],
               method      = "Friedman Two-Way Analysis of Variance By Ranks",
               conf.int    = c(NA, NA)
  )
  
  names(retval$null.value) <- "Friedman chi-square"
  names(retval$parameter) <- "null hypothesis Friedman chi-square"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
  
}