# #
# # Light. Measurements of Response agreement for Qualitative Data: Some Generalizations and Alternatives. 
# # Psychological Bulletin. 1971 76-5. pg 365-377 
# #
# 
# cor.cohen.kappa.onesample.1971.light <- function(
#     subject #
#    ,rater  #not used, standardized data input for kappa and Light's G
#    ,rating
#    ,weight = rep(1, length(rating))
#    ,alternative = c("two.sided", "greater", "less")
#    ,conf.level = .95
# ) {
#   validate.htest.alternative(alternative = alternative)
#   
#   if (any(weight < 0)) {
#     weight[which(weight < 0)] <- 0
#   }
# 
#   if (!is.factor(subject)) {
#     subject <- factor(subject)
#   }
# 
#   #if (!is.factor(rater)) {
#   #  rater <- factor(rater)
#   #}
# 
#   if (!is.factor(rating)) {
#     rating <- factor(rating)
#   }
# 
#   # N <- number of subjects
#   N <- length(levels(subject))
#   total_assignments <- sum(weight)
# 
#   #n_i  <- number of ratings for a subject
#   #n_j  <- number of ratings assigned to a category
#   #n_ij <- number of raters who assigned the ith subject to the jth category
# 
#   n_i <- list()
#   n_j <- list()
#   n_ij <- list()
# 
#   unique_subjects <- levels(subject)
#   unique_ratings <- levels(rating)
# 
#   for (i in unique_subjects) {
#     n_ij[[i]] <- list()
# 
#     for (j in unique_ratings) {
#       n_ij[[i]][[j]] <- 0
#     }
#   }
# 
#   for (i in unique_subjects) {
#     n_i[[i]] <- 0
#   }
# 
#   for (j in unique_ratings) {
#       n_j[[j]] <- 0
#   }
# 
#   for (idx in 1:length(weight)) {
#     this_subject <- unique_subjects[subject[idx]]
#     this_rating  <- unique_ratings[rating[idx]]
# 
#     n_ij[[this_subject]][[this_rating]] <- n_ij[[this_subject]][[this_rating]] + weight[idx]
#     n_i[[this_subject]] <- n_i[[this_subject]] + weight[idx]
#     n_j[[this_rating]]  <- n_j[[this_rating]]  + weight[idx] 
#   }
# 
#   #p_j <- proportion of all assignments made to the jth category
#   #p_j <- (1/(Nn))*sum[over subjects, i](n_ij), use n_j and total_assignments instead 
# 
#   p_j <- list()
#   for (j in unique_ratings) {
#       p_j[[j]] <- n_j[[j]] / total_assignments
#   }
# 
#   #P_i <- extent of agrement among n raters, proportion of agreeing pairs out of (n(n-1 possible))
# 
#   P_i <- list()
#   P_bar <- 0
# 
#   for (i in unique_subjects) {
#     P_i[[i]] <- 0
# 
#     for (j in unique_ratings) {
#       P_i[[i]] <- P_i[[i]] + (n_ij[[i]][[j]] * (n_ij[[i]][[j]]-1))
#     }
# 
#     P_i[[i]] <- P_i[[i]] / (n_i[[i]] * (n_i[[i]] - 1))
# 
#     P_bar <- P_bar + P_i[[i]]
#   }
# 
#   P_bar <- P_bar / N
# 
#   #P_bar_c <- mean proportion agreement expected by chance
#   #sum_p_j_cubed <- used for variance
#   P_bar_c <- 0
#   sum_p_j_cubed <- 0
# 
#   for (j in unique_ratings) {
#      P_bar_c <- P_bar_c + (p_j[[j]])^2
#      sum_p_j_cubed <- sum_p_j_cubed + (p_j[[j]])^3
#   }
# 
#   kappa <- (P_bar - P_bar_c) / (1 - P_bar_c)
#   #n for variance calculation, all subjects should have equal number of raters, but...
#   n_var <- mean(rmnames(unlist(n_i)))
#   se.kappa <- sqrt((2/(N*n_var*(n_var-1)) * 
#                    ((P_bar_c - ((2*n_var - 3) * (P_bar_c^2)) + ((2*(n_var - 2)*sum_p_j_cubed)))/
#                    ((1-P_bar_c)^2))))
# 
#   
# 
#   
#   #SE Kappa and CI
# 
#   z <- kappa/se.kappa
#  
#   cv      <- qnorm(conf.level+(1-conf.level)/2)
#   ci.add  <- cv*se.kappa
#   
#   ci.lower <- max(-1,kappa-ci.add)
#   ci.upper <- min(1,kappa+ci.add)
#   
#   p.value <- if (alternative[1] == "two.sided") {
#     tmp<-pnorm(z)
#     min(tmp,1-tmp)*2
#   } else if (alternative[1] == "greater") {
#     pnorm(z,lower.tail = FALSE)
#   } else if (alternative[1] == "less") {
#     pnorm(z,lower.tail = TRUE)
#   } else {
#     NA
#   }
#   
#   
#   
# 
#   retval<-list(data.name   = "raters, subjects, and ratings",
#                statistic   = z, 
#                estimate    = c(kappa = kappa 
#                                ,se.kappa = se.kappa
#                ),
#                parameter   = 0,
#                p.value     = p.value,
#                null.value  = 0,
#                alternative = alternative[1],
#                method      = "Cohen's Kappa (Light 1971 Extension)",
#                conf.int    = c(ci.lower, ci.upper)
#   )
#   
#   #names(retval$estimate) <- c("sample mean")
#   names(retval$statistic) <- "z"
#   names(retval$null.value) <- "kappa"
#   names(retval$parameter) <- "null hypothesis kappa"
#   attr(retval$conf.int, "conf.level")  <- conf.level
#   
#   class(retval)<-"htest"
#   retval
# }
# 
# 