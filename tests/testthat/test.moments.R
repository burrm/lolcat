context("Moment Tests - Skewness and Kurtosis")

test_that("values are close to other software", {
  
  round3 <- function(x) {round(x,3)}
  

  i1<-c(1,2,3,3,4,5)
  i2<-c(1,1,1,1,2,2,2,3,3,4)
  i3<-c(1,2,2,3,3,3,4,4,4,4)
  o1<- 0
  o2<- .712
  o3<- -.712
  
  expect_equal(o1, round3(skewness(i1)))
  expect_equal(o2, round3(skewness(i2)))
  expect_equal(o3, round3(skewness(i3)))
  
  rm(i1,i2,i3)
  rm(o1,o2,o3)
  
  
  i1<- c(1,2,3,3,4,5)
  i2<- c(1,2,3,4,5)
  i3<- c(3,1,3,2,4,5)
  o1<- -.3
  o2<- -1.2
  o3<- -.3
  
  expect_equal(o1, kurtosis(i1))
  expect_equal(o2, kurtosis(i2))
  expect_equal(o3, kurtosis(i3))
  
  rm(i1,i2,i3)
  rm(o1,o2,o3)

})


test_that("Normality testing from D'Agostino 1990 is consistent", {
  
  d <-c(393
        ,353
        ,334, 336
        ,327
        ,300, 300, 308 
        ,283, 285
        ,270,270,272, 278,278
        ,263,264,267,267,267,268
        ,254,254,254,256,256,258
        ,240,243,246,247,248
        ,230,230,230,230,231,232,232,232,234,234,236,236,238
        ,220,225,225,226
        ,210,211,212,215,216,217,218
        ,200,202
        ,192,198
        ,184
        ,167
        )
  
  expect_equal(62, length(d))
  expect_equal(250.0, round(mean(d),1))
  expect_equal(41.4, round(sqrt(var(d)),1))
  expect_equal(4.58, round(kurtosis(d, method = "pearson"),2))

  #kurtosis.test.new(d)
  
  #expect_equal(o1, kurtosis(i1))
  #expect_equal(o2, kurtosis(i2))
  #expect_equal(o3, kurtosis(i3))
  
  

})
