# # #Finite population modifier (IU 5)? - Sheskin 161 and 304 - use as adjustment to se.est
# # #Effect Size calculations (IU 7)
# #
# # #Sample sizes
# #
# # #Tests
# # # - one sample binomial test
# # # - two sample proportion (exact and approximate)
# # # - mcnemar's test
# # # - correlation tests
# #
# # #variance.test.twosample.independent(rnorm(50,10,1), rnorm(50,10,1), assume.normality = "no")
# set.seed(1)
# g1<-rnorm(50)
# g2<-rnorm(50,3,1)
# # g1<-g1[order(g1)]
# # g2<-g2[order(g2)]
# #
# # #cor(g1,g2)
# # #summary(lm(g1~g2))
# # variance.test.twosample.dependent(g1,g2, assume.normality = "no", non.norm.method = "dbar")
# 
# data<-data.frame(g1,g2)
# #
# summary.continuous(g1~1, data=data)
# skewness.test(g1, method = "z")
# #
# # edit(poisson.test)
# #
# write.table(g1, file="~/devnull/skew.dat", row.names = F)
# 
# require(MASS)
# ?target





#http://blog.abhranil.net/2014/02/08/r-code-for-multivariate-random-walk-metropolis-hastings-sampling/
