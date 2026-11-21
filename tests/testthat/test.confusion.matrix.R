require(lolcat)

context("Confusion Matrix - Diagnostic Odds Ratio")

test_that("Test case 1", {
  expect_equal(34.667, round(confusion.matrix.diagnostic.odds.ratio.simple(
    true.positive = 26,
    false.positive = 12,
    false.negative = 3,
    true.negative = 48
  ),3))
  
})


test_that("Test case 2", {
  #                 TP       FP        FN          TN
  actual    <- c(rep(1,26),rep(0,12), rep(1,3), rep(0,48))
  predicted <- c(rep(1,26),rep(1,12), rep(0,3), rep(0,48))
  
  cm <- confusion.matrix(actual = actual, predicted = predicted)
  
  expect_equal(26, confusion.matrix.true.positive(cm))
  expect_equal(12, confusion.matrix.false.positive(cm))
  expect_equal(48, confusion.matrix.true.negative(cm))
  expect_equal(3, confusion.matrix.false.negative(cm))
  
  expect_equal(34.667, round(confusion.matrix.diagnostic.odds.ratio(cm),3))
  
})

#https://en.wikipedia.org/wiki/Receiver_operating_characteristic
context("Confusion Matrix - Other Calculations")

test_that("Example A", {
  #                  TP       FP        FN          TN
  actual    <- c(rep(1,63),rep(0,28), rep(1,37), rep(0,72))
  predicted <- c(rep(1,63),rep(1,28), rep(0,37), rep(0,72))
  
  cm <- confusion.matrix(actual = actual, predicted = predicted)
  
  expect_equal(63, confusion.matrix.true.positive(cm))
  expect_equal(28, confusion.matrix.false.positive(cm))
  expect_equal(72, confusion.matrix.true.negative(cm))
  expect_equal(37, confusion.matrix.false.negative(cm))
  
  expect_equal(.63, round(confusion.matrix.true.positive.rate(cm),2))
  expect_equal(.28, round(confusion.matrix.false.positive.rate(cm),2))
  expect_equal(.69, round(confusion.matrix.positive.predictive.value(cm),2))
  expect_equal(.66, round(confusion.matrix.f1.score(cm),2))
  expect_equal(.68, round(confusion.matrix.accuracy(cm),2))
  
})



test_that("Example B", {
  #                  TP       FP        FN          TN
  actual    <- c(rep(1,77),rep(0,77), rep(1,23), rep(0,23))
  predicted <- c(rep(1,77),rep(1,77), rep(0,23), rep(0,23))
  
  cm <- confusion.matrix(actual = actual, predicted = predicted)
  
  expect_equal(77, confusion.matrix.true.positive(cm))
  expect_equal(77, confusion.matrix.false.positive(cm))
  expect_equal(23, confusion.matrix.true.negative(cm))
  expect_equal(23, confusion.matrix.false.negative(cm))
  
  expect_equal(.77, round(confusion.matrix.true.positive.rate(cm),2))
  expect_equal(.77, round(confusion.matrix.false.positive.rate(cm),2))
  expect_equal(.50, round(confusion.matrix.positive.predictive.value(cm),2))
  expect_equal(.61, round(confusion.matrix.f1.score(cm),2))
  expect_equal(.50, round(confusion.matrix.accuracy(cm),2))
  
})


test_that("Example C", {
  #                  TP       FP        FN          TN
  actual    <- c(rep(1,24),rep(0,88), rep(1,76), rep(0,12))
  predicted <- c(rep(1,24),rep(1,88), rep(0,76), rep(0,12))
  
  cm <- confusion.matrix(actual = actual, predicted = predicted)
  
  expect_equal(24, confusion.matrix.true.positive(cm))
  expect_equal(88, confusion.matrix.false.positive(cm))
  expect_equal(12, confusion.matrix.true.negative(cm))
  expect_equal(76, confusion.matrix.false.negative(cm))
  
  expect_equal(.24, round(confusion.matrix.true.positive.rate(cm),2))
  expect_equal(.88, round(confusion.matrix.false.positive.rate(cm),2))
  expect_equal(.21, round(confusion.matrix.positive.predictive.value(cm),2))
  expect_equal(.23, round(confusion.matrix.f1.score(cm),2))
  expect_equal(.18, round(confusion.matrix.accuracy(cm),2))
  
})



test_that("Example D", {
  #                  TP       FP        FN          TN
  actual    <- c(rep(1,76),rep(0,12), rep(1,24), rep(0,88))
  predicted <- c(rep(1,76),rep(1,12), rep(0,24), rep(0,88))
  
  cm <- confusion.matrix(actual = actual, predicted = predicted)
  
  expect_equal(76, confusion.matrix.true.positive(cm))
  expect_equal(12, confusion.matrix.false.positive(cm))
  expect_equal(88, confusion.matrix.true.negative(cm))
  expect_equal(24, confusion.matrix.false.negative(cm))
  
  expect_equal(.76, round(confusion.matrix.true.positive.rate(cm),2))
  expect_equal(.12, round(confusion.matrix.false.positive.rate(cm),2))
  expect_equal(.86, round(confusion.matrix.positive.predictive.value(cm),2))
  expect_equal(.81, round(confusion.matrix.f1.score(cm),2))
  expect_equal(.82, round(confusion.matrix.accuracy(cm),2))
  
})


