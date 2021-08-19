#
# Light. Measurements of Response agreement for Qualitative Data: Some Generalizations and Alternatives. 
# Psychological Bulletin. 1971 76-5. pg 365-377 
#

#' Light's G   
#' 
#' Calculate Light's G based on Light's 1971 paper. Used to compare multiple raters with standard.
#'
#' @param subject A vector identifying subjects.
#' @param rater A vector identifying raters. 
#' @param rating A vector identifying ratings.
#' @param rater.standard A scalar identifying the name of the rater considered "correct".
#' @param alternative The alternative hypothesis to use for the test computation.
#' @param conf.level The confidence level for this test, between 0 and 1.
#'
#' @return The results of the statistical test.

#Note: Include standard as a rater in the data and identify it with rater.standard
cor.light.g.onesample <- function(
    subject 
   ,rater  
   ,rating
   ,rater.standard = "Standard"
   #,weight = rep(1, length(rating)) -> not used, only 1
   ,alternative = c("two.sided", "greater", "less")
   ,conf.level = .95
) {
  validate.htest.alternative(alternative = alternative)
  
  #if (any(weight < 0)) {
  #  weight[which(weight < 0)] <- 0
  #}

  if (!is.factor(subject)) {
    subject <- factor(subject)
  }

  if (!is.factor(rater)) {
    rater <- factor(rater)
  }

  if (!is.factor(rating)) {
    rating <- factor(rating)
  }

  # m observers assign n subjects among C categories
  
  unique_subjects <- levels(subject)
  unique_ratings <- levels(rating)
  unique_raters <- levels(rater)

  if (!(rater.standard %in% unique_raters)) {
    stop("rater.standard not in list of raters. Specify rater.standard to be one of the names in the rater parameter.")
  }

  #n_ij_p <- responses in the pth contingency table, i,j in 1:C and p in 1:m

  n_ij_p <- list() #n_ij_p[[rater]][[row_category]][[col_category]] <- count

  for (p in unique_raters) {
    if (p != rater.standard) {
      n_ij_p[[p]] <- list()
      for (i in unique_ratings) {
        n_ij_p[[p]][[i]] <- list()
        for (j in unique_ratings) {
          n_ij_p[[p]][[i]][[j]] <- 0
        }
      }
    }
  }

  n <- 0

  standard        <- list() #[[subject]]         <- rating
  standard.margin <- list() #[[rating]]          <- n_i+ row sum
  rater.margin    <- list() #[[rater]][[rating]] <- n_+j col sum

  rater.names   <- levels(rater)[rater]
  subject.names <- levels(subject)[subject]
  rating.names  <- levels(rating)[rating]

  for (i in unique_ratings) {
    standard.margin[[i]] <- 0
  }

  for (p in unique_raters) {
    if (p != rater.standard) {
      rater.margin[[p]] <- list()
      for (j in unique_ratings) {
        rater.margin[[p]][[j]] <- 0
      }
    }
  }

  for (idx in 1:length(rater.names)) {
    if (rater.names[idx] == rater.standard) {
      standard[[subject.names[idx]]] <- rating.names[idx]
      standard.margin[[rating.names[idx]]] <- standard.margin[[rating.names[idx]]] + 1 
      n <- n + 1
    } else {
      rater.margin[[rater.names[idx]]][[rating.names[idx]]] <- rater.margin[[rater.names[idx]]][[rating.names[idx]]] + 1
    }
  }

  for (idx in 1:length(rater.names)) {
    tmp.sn <- subject.names[idx]
    p <- rater.names[idx]
    j <- rating.names[idx]
    i <- standard[[tmp.sn]]

    n_ij_p[[p]][[i]][[j]] <- n_ij_p[[p]][[i]][[j]] + 1
  }

  #row variable is always standard, col variable is always a rater
  #notation n_i+ is row margin, n_+j is column margin
  #G = (t_m - E(t_m))/sqrt(xi_t_m) ~ standard normal [Eq 11]

  #t_m - across all raters, how many total agreements with standard, eq 11a
  #E(t_m) - expected value for total agreements with standard, eq 11b

  t_m <- 0
  E_t_m <- 0

  xi_t_m_3 <- 0 #convenient to calculate with this loop
  xi_t_m_4 <- 0 #convenient to calculate with this loop

  for (i in unique_ratings) {
    for (p in unique_raters) {
      if (p != rater.standard) {
        t_m <- t_m + n_ij_p[[p]][[i]][[i]]
      } 
    }

    E_t_m <- E_t_m + standard.margin[[i]] * sum(rmnames(unlist(lapply(rater.margin, FUN = function(margin) {
      margin[[i]]
    }))))

    xi_t_m_3 <- xi_t_m_3 + (standard.margin[[i]] * standard.margin[[i]]) * sum(rmnames(unlist(lapply(rater.margin, FUN = function(margin) {
      margin[[i]]
    }))))

    xi_t_m_4 <- xi_t_m_4 + standard.margin[[i]] * sum(rmnames(unlist(lapply(rater.margin, FUN = function(margin) {
      margin[[i]] * margin[[i]]
    }))))
  }

  E_t_m <- E_t_m / n
  
  xi_t_m_3 <- xi_t_m_3 /(n*(n-1))
  xi_t_m_4 <- xi_t_m_4 /(n*(n-1))

  #xi_t_m - variance of t_m, eq 11c
  xi_t_m_1 <- (n/(n-1)) * E_t_m

  xi_t_m_Z <- 0

  #outer loop - standard margin
  #inner loop - rater margin
  #lappy/sum over raters for product with inner, then outer loop

  for (i in unique_ratings) {
    add_outer <- 0
    for (j in unique_ratings) {
      add_outer <- add_outer + standard.margin[[j]] * sum(rmnames(unlist( lapply(rater.margin, FUN = function(p) {
        p[[i]] * p[[j]]
      }))))
    }
    add_outer <- add_outer * standard.margin[[i]]
    xi_t_m_Z <- xi_t_m_Z + add_outer
  }

  xi_t_m_2 <- xi_t_m_Z / ( (n^2) * (n-1))
  xi_t_m <- xi_t_m_1 + xi_t_m_2 - xi_t_m_3 - xi_t_m_4
  
  #Delete/rename most below this line

  #n for variance calculation, all subjects should have equal number of raters, but...
  se.G <- sqrt(xi_t_m)
  G <- (t_m-E_t_m)/se.G
   
  #SE Kappa and CI

  z <- G
 
  cv      <- qnorm(conf.level+(1-conf.level)/2)
  ci.add  <- cv*se.G #+ E_t_m #note: not 100% sure about adding back expected value
  
  ci.lower <- NA# G-ci.add
  ci.upper <- NA# G+ci.add
  
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
  
  
  

  retval<-list(data.name   = "standard, subjects, raters, and ratings",
               statistic   = z, 
               estimate    = c(G = G 
                               ,se.G = se.G
               ),
               parameter   = 0,
               p.value     = p.value,
               null.value  = 0,
               alternative = alternative[1],
               method      = "Light's G (1971)",
               conf.int    = c(ci.lower, ci.upper)
  )
  
  #names(retval$estimate) <- c("sample mean")
  names(retval$statistic) <- "z"
  names(retval$null.value) <- "G"
  names(retval$parameter) <- "null hypothesis G"
  attr(retval$conf.int, "conf.level")  <- conf.level
  
  class(retval)<-"htest"
  retval
}

