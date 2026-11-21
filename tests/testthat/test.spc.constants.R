require(lolcat)

context("SPC Bias Correction Constants")

test_that("SPC constants for d2 are close to table values", {
  expect_equal(round(spc.constant.calculation.d2(2),3), 1.128)
  expect_equal(round(spc.constant.calculation.d2(10),3), 3.078)
  expect_equal(round(spc.constant.calculation.d2(20),3), 3.735)
  expect_equal(round(spc.constant.calculation.d2(100),3), 5.015)
})

test_that("SPC constants for c2 are close to table values", {
  expect_equal(round(spc.constant.calculation.c2(2),4), .5642)
  expect_equal(round(spc.constant.calculation.c2(10),4), .9227)
  expect_equal(round(spc.constant.calculation.c2(20),4), .9619)
  expect_equal(round(spc.constant.calculation.c2(100),4), .9925)
})


test_that("SPC constants for c4 are close to table values", {
  expect_equal(round(spc.constant.calculation.c4(2),4), .7979)
  expect_equal(round(spc.constant.calculation.c4(10),4), .9727)
  expect_equal(round(spc.constant.calculation.c4(20),4), .9869)
  expect_equal(round(spc.constant.calculation.c4(100),4), .9975)
})


test_that("SPC constants for d3 are close to table values", {
  expect_equal(round(spc.constant.calculation.d3(2),4), .8525)
  expect_equal(round(spc.constant.calculation.d3(10),4), .7971)
  expect_equal(round(spc.constant.calculation.d3(20),4), .7287)
  expect_equal(round(spc.constant.calculation.d3(100),4), .6052)
})

test_that("SPC constants for d4 are close to table values", {
  expect_equal(round(spc.constant.calculation.d4(2),3), 0.954)
  expect_equal(round(spc.constant.calculation.d4(10),3), 3.024)
  expect_equal(round(spc.constant.calculation.d4(20),3), 3.686)
  expect_equal(round(spc.constant.calculation.d4(100),3), 4.968)
})


test_that("SPC constants for A2 are close to table values", {
  expect_equal(round(spc.constant.calculation.A2(2),3), 1.880)
  expect_equal(round(spc.constant.calculation.A2(5),3), 0.577)
  expect_equal(round(spc.constant.calculation.A2(10),3), .308)
  expect_equal(round(spc.constant.calculation.A2(15),3), .223)
})



test_that("SPC constants for D3 are close to table values", {
  expect_equal(is.na(round(spc.constant.calculation.D3(2),3)), TRUE)
  expect_equal(is.na(round(spc.constant.calculation.D3(6),3)), TRUE)
  expect_equal(round(spc.constant.calculation.D3(7),3), .076)
  expect_equal(round(spc.constant.calculation.D3(13),3), .307)
})


test_that("SPC constants for D4 are close to table values", {
  expect_equal(round(spc.constant.calculation.D4(2),3), 3.267) #Note: 3.268 in Wheeler
  expect_equal(round(spc.constant.calculation.D4(6),3), 2.004)
  expect_equal(round(spc.constant.calculation.D4(7),3), 1.924)
  expect_equal(round(spc.constant.calculation.D4(13),3), 1.693)
})


test_that("SPC constants for A4 are close to table values", {
  expect_equal(round(spc.constant.calculation.A4(2),3), 2.224)
  expect_equal(round(spc.constant.calculation.A4(5),3), 0.594)
  expect_equal(round(spc.constant.calculation.A4(8),3), .380)
  expect_equal(round(spc.constant.calculation.A4(10),3), .314)
})


test_that("SPC constants for D5 are close to table values", {
  expect_equal(is.na(round(spc.constant.calculation.D5(2),3)), TRUE)
  expect_equal(is.na(round(spc.constant.calculation.D5(6),3)), TRUE)
#  expect_equal(round(spc.constant.calculation.D5(7),3), .078)
#  expect_equal(round(spc.constant.calculation.D5(10),3), .227)
})

test_that("SPC constants for D6 are close to table values", {
#  expect_equal(round(spc.constant.calculation.D6(2),3), 3.865)
  expect_equal(round(spc.constant.calculation.D6(6),3), 2.055) #note: 2.055 in Wheeler
  expect_equal(round(spc.constant.calculation.D6(8),3), 1.901) #note: 1.901 in Wheeler
  expect_equal(round(spc.constant.calculation.D6(10),3), 1.809)
})

test_that("SPC constants for A1 are close to table values", {
  expect_equal(round(spc.constant.calculation.A1(2),3), 3.760)
  expect_equal(round(spc.constant.calculation.A1(5),3), 1.596) #note: 1.595 in Wheeler
  expect_equal(round(spc.constant.calculation.A1(10),3), 1.028)
  expect_equal(round(spc.constant.calculation.A1(15),3), 0.816)
})

test_that("SPC constants for B3 are close to table values", {
  expect_equal(is.na(round(spc.constant.calculation.B3(2),3)), TRUE)
  expect_equal(round(spc.constant.calculation.B3(6),3), .030)
  expect_equal(round(spc.constant.calculation.B3(10),3), .284)
  expect_equal(round(spc.constant.calculation.B3(15),3), .428)
})

test_that("SPC constants for B4 are close to table values", {
  expect_equal(round(spc.constant.calculation.B4(2),3), 3.267)
  expect_equal(round(spc.constant.calculation.B4(6),3), 1.970)
  expect_equal(round(spc.constant.calculation.B4(10),3), 1.716)
  expect_equal(round(spc.constant.calculation.B4(15),3), 1.572)
})

test_that("SPC constants for A3 are close to table values", {
  expect_equal(round(spc.constant.calculation.A3(2),3), 2.659)
  expect_equal(round(spc.constant.calculation.A3(5),3), 1.427)
  expect_equal(round(spc.constant.calculation.A3(10),3), 0.975)
  expect_equal(round(spc.constant.calculation.A3(15),3), 0.789)
})
