require(lolcat)

context("Dispersion Tests - ADA/ADM/ADMn-1")

test_that("values are close to other software", {

  i1<-c(1,2,3,3,4,5)
  i2<-c(1,2,3,4,5)
  i3<-c(3,1,3,2,4,5)
  
  o1<-c(2,1,NA,0,1,2)
  o2<-c(2,1,NA,1,2)
  o3<-c(NA,2,0,1,1,2)
  
  expect_equal(o1,dispersion.ADMn1(i1))
  expect_equal(o2,dispersion.ADMn1(i2))
  expect_equal(o3,dispersion.ADMn1(i3))
  
  rm(i1,i2,i3)
  rm(o1,o2,o3)

})
