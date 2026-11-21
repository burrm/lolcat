#
# Fleiss, Nee, Landis. Large Sample Variance of Kappa in the Case of Different Sets of Raters. 
# Psychological Bulletin. 1979 86-5. pg 974-977 
#

#' Cohen's Kappa   
#' 
#' Calculate Fleiss' extension of Cohen's Kappa based on Fleiss', Nee's, and Landis' 1979 paper.
#'
#' @param subject A vector identifying subjects.
#' @param rater A vector identifying raters. Not used in this calculation.
#' @param rating A vector identifying ratings.
#' @param weight A vector identifying weights.
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return The results of the statistical test.

cor.cohen.kappa.onesample.1979.fleiss <- function(
    subject #
   ,rater  #not used, standardized data input for kappa and Light's G
   ,rating
   ,weight = rep(1, length(rating))
   ,alternative = c("two.sided", "greater", "less")
   ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  if (any(weight < 0)) {
    weight[which(weight < 0)] <- 0
  }

  if (!is.factor(subject)) {
    subject <- factor(subject)
  }

  #if (!is.factor(rater)) {
  #  rater <- factor(rater)
  #}

  if (!is.factor(rating)) {
    rating <- factor(rating)
  }

  # N <- number of subjects
  N <- length(levels(subject))
  total_assignments <- sum(weight)

  #n_i  <- number of ratings for a subject
  #n_j  <- number of ratings assigned to a category
  #n_ij <- number of raters who assigned the ith subject to the jth category

  n_i <- list()
  n_j <- list()
  n_ij <- list()

  unique_subjects <- levels(subject)
  unique_ratings <- levels(rating)

  for (i in unique_subjects) {
    n_ij[[i]] <- list()

    for (j in unique_ratings) {
      n_ij[[i]][[j]] <- 0
    }
  }

  for (i in unique_subjects) {
    n_i[[i]] <- 0
  }

  for (j in unique_ratings) {
      n_j[[j]] <- 0
  }

  for (idx in 1:length(weight)) {
    this_subject <- unique_subjects[subject[idx]]
    this_rating  <- unique_ratings[rating[idx]]

    n_ij[[this_subject]][[this_rating]] <- n_ij[[this_subject]][[this_rating]] + weight[idx]
    n_i[[this_subject]] <- n_i[[this_subject]] + weight[idx]
    n_j[[this_rating]]  <- n_j[[this_rating]]  + weight[idx] 
  }

  #p_j <- proportion of all assignments made to the jth category
  #p_j <- (1/(Nn))*sum[over subjects, i](n_ij), use n_j and total_assignments instead 
  #q_j <- 1-p_j, used in variance

  p_j <- list()
  q_j <- list()
  for (j in unique_ratings) {
      p_j[[j]] <- n_j[[j]] / total_assignments
      q_j[[j]] <- 1 - p_j[[j]]
  }

  #n for variance and k_j calculation, all subjects should have equal number of raters, but...
  n_var <- mean(rmnames(unlist(n_i)))

  #k_j - measure of agreement for category j above chance
  k_j <- list()
  kappa_num <- 0
  kappa_denom <- 0

  for (j in unique_ratings) {
    k_j[[j]] <- (1-((1/(N*n_var*(n_var-1)*p_j[[j]]*q_j[[j]])) 
                    * (sum(rmnames(unlist(lapply(n_ij, FUN = function(i) {i[[j]] * (n_var-i[[j]]) })))))))

    kappa_num <- kappa_num + p_j[[j]]*q_j[[j]]*k_j[[j]]
    kappa_denom <- kappa_denom + p_j[[j]]*q_j[[j]]
  }

  #equation 3 from paper
  kappa <- kappa_num / kappa_denom

  #variance calculation (equation 12)
  se.comp.1 <- 0 #sum[over j] p_j*q_j
  se.comp.2 <- 0 #sum[over j] p_j*q_j*(q_j-p_j)

  for (j in unique_ratings) {
    se.comp.1 <- se.comp.1 + p_j[[j]]*q_j[[j]]
    se.comp.2 <- se.comp.2 + p_j[[j]]*q_j[[j]]*(q_j[[j]]-p_j[[j]])
  }

  se.kappa <- sqrt((2/(N*n_var*(n_var-1)*(se.comp.1^2)))*((se.comp.1^2)-se.comp.2))

  #calculation of z, equation 17
  z.add <- 1/(N*(n_var-1))

   
  #SE Kappa and CI

  z <- (kappa + z.add)/se.kappa
 
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  ci.add  <- (cv*se.kappa - z.add) #note: not 100% sure about subtracting z.add
  
  ci.lower <- max(-1,kappa-ci.add)
  ci.upper <- min(1,kappa+ci.add)
  
  p.value <- if (alternative[1] == "two.sided") {
    tmp<-pnorm(z)
    min(tmp,1-tmp)*2
  } else if (alternative[1] == "greater") {
    pnorm(z,lower.tail = FALSE)
  } else if (alternative[1] == "less") {
    pnorm(z,lower.tail = TRUE)
  } else {
    NA
  }
  
  
  

  retval<-list(data.name   = "subjects and ratings",
               statistic   = z, 
               estimate    = c(kappa = kappa 
                               ,se.kappa = se.kappa
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Cohen's Kappa (Fleiss, Nee, Landis 1979)",
               conf.int    = c(ci.lower, ci.upper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "kappa"
  names(retval$parameter) <- "null hypothesis kappa"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}
