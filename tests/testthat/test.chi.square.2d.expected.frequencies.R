require(lolcat)

context("Chi-Square - Expected Cell Frequencies")

test_that("Sheskin - Noise vs. Altruism - Pg 639", {
  m <- matrix(c(30,70,60,40), byrow = T, nrow = 2)
  exp.freq <- chi.square.2d.expected.frequencies(m)
  
  expect_equal(45, round(exp.freq[1,1],2))
  expect_equal(55, round(exp.freq[1,2],2))
  
  expect_equal(45, round(exp.freq[2,1],2))
  expect_equal(55, round(exp.freq[2,2],2))
  
})


test_that("Sheskin - Rodent vs. Biter - Pg 665", {
  m <- matrix(c(20,16,24,30,10,10,50,30,10,19,11,50), byrow = T, nrow = 4)
  exp.freq <- chi.square.2d.expected.frequencies(m)
  
  expect_equal(25.50, round(exp.freq[1,1],2))
  expect_equal(14.36, round(exp.freq[1,2],2))
  expect_equal(20.14, round(exp.freq[1,3],2))
  
  expect_equal(21.25, round(exp.freq[2,1],2))
  expect_equal(11.96, round(exp.freq[2,2],2))
  expect_equal(16.79, round(exp.freq[2,3],2))
  
  expect_equal(38.25, round(exp.freq[3,1],2))
  expect_equal(21.54, round(exp.freq[3,2],2))
  expect_equal(30.21, round(exp.freq[3,3],2))
  
  expect_equal(34, round(exp.freq[4,1],2))
  expect_equal(19.14, round(exp.freq[4,2],2))
  expect_equal(26.86, round(exp.freq[4,3],2))
  
  
})


test_that("Sheskin - Kappa - Pg 688", {
  m <- matrix(c(31,4,2,6,29,8,10,7,3), byrow = T, nrow = 3)
  exp.freq <- chi.square.2d.expected.frequencies(m)
  
  expect_equal(17.39, round(exp.freq[1,1],2))
  expect_equal(17.2, round(exp.freq[2,2],2))
  expect_equal(2.6, round(exp.freq[3,3],2))
  
})
