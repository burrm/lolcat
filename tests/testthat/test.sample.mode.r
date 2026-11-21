require(lolcat)
#require(testthat)
context("Sample Mode")

test_that("values are accurate and return the correct type", {
  
  #numeric

  i0<-c(1,2,3,3,4,5) #-> 3
  o0 <- 3
  
  i1<-c(1,2,2,3,3,4,5) #-> 2,3
  o1 <- c(2,3)
  
  i2<-c(1,2,3,4,5)   #-> NA
  o2 <- NA
  
  #character
  
  i3 <- c("a","b","b","c","c","c","d","d","e") #-> "c"
  o3 <- "c"
  
  i4 <- c("a","b","b","b","c","c","c","d","d","e") #-> "b","c"
  o4 <- c("b","c")
  
  i5 <- c("a","b","c","d","e") #-> NA
  o5 <- NA
  
  # factor
  
  i6 <- factor(i3) # 3 levels a-e
  o6 <- factor("c", levels = c("a","b","c","d","e"))
  
  i7 <- factor(i4) # 2,3 levels a-e 
  o7 <- factor(c("b","c"), levels = c("a","b","c","d","e"))
  
  #i8 <- factor(i5) # NA
  #o8 <- factor(NA, levels = c("a","b","c","d","e"))
  
  levels_i <- levels(i6)
  out <- NA
  
  out <- sample.mode(i0)
  expect_identical(out, o0)
  
  out <- sample.mode(i1)
  expect_identical(out, o1)
  
  out <- sample.mode(i2)
  expect_identical(out, o2)
  
  out <- sample.mode(i3)
  expect_identical(out, o3)
  
  out <- sample.mode(i4)
  expect_identical(out, o4)
  
  out <- sample.mode(i5)
  expect_identical(out, o5)
  
  out <- sample.mode(i6)
  expect_identical(out, o6)
  
  out <- sample.mode(i7)
  expect_identical(out, o7)
  
  rm(i0,i1,i2,i3,i4,i5,i6,i7) #,i8
  rm(o0,o1,o2,o3,o4,o5,o6,o7) #,o8
  rm(levels_i, out)
})
