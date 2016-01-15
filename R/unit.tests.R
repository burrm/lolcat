require(RUnit)

test.rmnames<-function() {
  checkEquals(NULL, names(rmnames(c(a=2))))
}

test.dispersion.ADMn1 <- function() {
  i1<-c(1,2,3,3,4,5)
  i2<-c(1,2,3,4,5)
  i3<-c(3,1,3,2,4,5)
  
  o1<-c(2,1,NA,0,1,2)
  o2<-c(2,1,NA,1,2)
  o3<-c(NA,2,0,1,1,2)
  
  checkIdentical(o1,dispersion.ADMn1(i1))
  checkIdentical(o2,dispersion.ADMn1(i2))
  checkIdentical(o3,dispersion.ADMn1(i3))
  
  rm(i1,i2,i3)
  rm(o1,o2,o3)
}


test.skewness <- function() {
  i1<-c(1,2,3,3,4,5)
  i2<-c(1,1,1,1,2,2,2,3,3,4)
  i3<-c(1,2,2,3,3,3,4,4,4,4)
  o1<- 0
  o2<- .712
  o3<- -.712
  
  checkEquals(o1, skewness(i1), tolerance = .0009)
  checkEquals(o2, skewness(i2), tolerance = .0009)
  checkEquals(o3, skewness(i3), tolerance = .0009)
  
  rm(i1,i2,i3)
  rm(o1,o2,o3)
  
}

test.kurtosis <- function() {
  i1<- c(1,2,3,3,4,5)
  i2<- c(1,2,3,4,5)
  i3<- c(3,1,3,2,4,5)
  o1<- -.3
  o2<- -1.2
  o3<- -.3
  
  checkEquals(o1, kurtosis(i1))
  checkEquals(o2, kurtosis(i2))
  checkEquals(o3, kurtosis(i3))
  
  rm(i1,i2,i3)
  rm(o1,o2,o3)
}


#TODO: Convert to sk/ku unit test
#set.seed(1)
#skewness.test(rnorm(50))
#set.seed(1)
#kurtosis.test(rnorm(50))

#poisson.test.twosample.simple(sample.count.g1 = 10, n.g1 = 100, sample.count.g2 = 25, n.g2 = 2000)
#poisson.test.onesample.simple(15, 1)
#?poisson.test
