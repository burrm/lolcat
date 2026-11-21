require(lolcat)

context("Shapiro Wilk Exponentiality Tests")

test_that("values are close to Shapiro and Wilk (1972)", {
  #Pg 359 Illustration
  
  pg.359.input <- c(6,1,-4,8,-2,5,0)
  pg.359.output <- shapiro.wilk.exponentiality.test(pg.359.input)
  
  expect_equal(c(W = .356), round(pg.359.output$statistic[1],3))  
  expect_equal(T, pg.359.output$p.value >= .1)
  
  #Example 1
  
  ex.1.input <- c(1,4,5,6,15,20,40,40,60,93,95,106,125,151,200,268,459,827,840,1089)
  ex.1.output <- shapiro.wilk.exponentiality.test(ex.1.input)
  
  expect_equal(c(W = .0259), round(ex.1.output$statistic[1],4))  
  expect_equal(T, ex.1.output$p.value < .05)

  #Example 2  
  
  ex.2.input <- c(15.25, 22.16, 32.62, 63.97, 65.65, 69.80,
                  85.80, 98.38, 113.07, 138.38, 154.06, 167.83, 
                  180.90, 284.69
  )
  
  ex.2.output <- shapiro.wilk.exponentiality.test(ex.2.input)
  
  expect_equal(c(W = .127), round(ex.2.output$statistic[1],3))  
  expect_equal(T, ex.2.output$p.value >= .3 & ex.2.output$p.value <= .4)
  
})
