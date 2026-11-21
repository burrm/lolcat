require(lolcat)

context("Exact Proportion Tests - One Sample and Two Sample")

test_that("proportion test results are close to other software", {

  # TODO: Integrate test below from MVPStats
  # np <- 1:10
  # 
  # d <- data.frame(n = np, p = dbinom(x = np, prob = .5, size = 10))
  # 
  # d$cdf <- cumsum(d$p)
  # 
  # d$pbin_lt <- pbinom(q = np,size = 10,prob = .5)
  # 
  # d$pbin_ut <- pbinom(q = np,size = 10,prob = .5, lower.tail = F)
  # 
  # d$pbin_ut2 <- d$pbin_ut + d$p
  # 
  # d$mvp_2t <- c(.021, .109, .344, .754, 1, .754, .344, .109, .021, .002)
  # 
  # d$mvp_lt <- c(.011, .055, .172, .377, .623, .828, .945, .989, .999, 1)
  # 
  # d$mvp_gt <- c(.999, .989, .945, .828, .623, .377, .172, .055, .011, .001)
  # 
  # round(d,3)
  # 
  
  
  
  
  
  
  
  
  
  p.test <- proportion.test.onesample.exact.simple(sample.proportion = 5/25,
                                         sample.size = 25,
                                         null.hypothesis.proportion = .5
                                         )
  
  expect_equal( round(p.test$p.value,3) , .004)
  expect_equal( round(p.test$conf.int[1],4) , .0683)
  expect_equal( round(p.test$conf.int[2],4) , .4070)

  
  p.test <- proportion.test.onesample.exact.simple(sample.proportion = 5/25,
                                                   sample.size = 25,
                                                   null.hypothesis.proportion = .5
                                                   ,alternative = "less"
  )
  
  expect_equal( round(p.test$p.value,3) , .002)
  expect_equal( round(p.test$conf.int[1],4) , .0683)
  expect_equal( round(p.test$conf.int[2],4) , .4070)
  
  
  
  p.test <- proportion.test.onesample.exact.simple(sample.proportion = 5/25,
                                                   sample.size = 25,
                                                   null.hypothesis.proportion = .5
                                                   ,alternative = "greater"
  )
  
  
  expect_equal( round(p.test$p.value,2) , 1)
  expect_equal( round(p.test$conf.int[1],4) , .0683)
  expect_equal( round(p.test$conf.int[2],4) , .4070)
  
  
  
  
  
  p.test <- proportion.test.onesample.exact.simple(sample.proportion = 20/25,
                                                   sample.size = 25,
                                                   null.hypothesis.proportion = .5
  )
  
  
  #expect_equal( round(p.test$p.value,3) , .004)
  expect_equal( round(p.test$conf.int[1],4) , .5930)
  expect_equal( round(p.test$conf.int[2],4) , .9317)
  
  
  p.test <- proportion.test.onesample.exact.simple(sample.proportion = 20/25,
                                                   sample.size = 25,
                                                   null.hypothesis.proportion = .5,
                                                   alternative = "less"
  )
  
  
  expect_equal( round(p.test$p.value,2) , 1)
  expect_equal( round(p.test$conf.int[1],4) , .5930)
  expect_equal( round(p.test$conf.int[2],4) , .9317)
  
  
  
  p.test <- proportion.test.onesample.exact.simple(sample.proportion = 20/25,
                                                   sample.size = 25,
                                                   null.hypothesis.proportion = .5,
                                                   alternative = "greater"
  )
  
  
  #expect_equal( round(p.test$p.value,3) , .002)
  expect_equal( round(p.test$conf.int[1],4) , .5930)
  expect_equal( round(p.test$conf.int[2],4) , .9317)
  
  
  
  
  # Start two sample tests  
  
  
  p.test <- proportion.test.twosample.exact.simple(np.g1 = 20,
                                                   sample.size.g1 = 25,
                                                   np.g2 = 14,
                                                   sample.size.g2 = 25,
                                                   alternative = "two.sided"
  )
  
  expect_equal( round(p.test$p.value,3) , .128)
  
  

  p.test <- proportion.test.twosample.exact.simple(np.g1 = 20,
                                                   sample.size.g1 = 25,
                                                   np.g2 = 14,
                                                   sample.size.g2 = 25,
                                                   alternative = "less"
  )
  
  expect_equal( round(p.test$p.value,3) , .984)
  

  
    
  p.test <- proportion.test.twosample.exact.simple(np.g1 = 20,
                                                   sample.size.g1 = 25,
                                                   np.g2 = 14,
                                                   sample.size.g2 = 25,
                                                   alternative = "greater"
  )
  
  expect_equal( round(p.test$p.value,3) , .064)
  
})
