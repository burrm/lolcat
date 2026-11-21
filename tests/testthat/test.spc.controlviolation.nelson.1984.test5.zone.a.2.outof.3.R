require(lolcat)

context("SPC Nelson 1984 Test 5")

test_that("Nelson Test 5 2/3 Zone A or beyond", {
  expect_equal(
      c(T,T,F,F,F,F,T,T,T,F,F,T,T,T,F)
      ,spc.controlviolation.nelson.1984.test5.zone.a.2.outof.3(
        chart.series = c(2.5,2.5,0,1,-1,1,-2.5,1,-2.5,1,-1.5,1.5,-2.1,-2.1,-1),
  
        center.line = rep(0,15),
  
        control.limits.ucl = rep(3,15),
        zone.a.upper = rep(3,15),
        zone.ab.upper = rep(2,15),
        zone.bc.upper = rep(1,15),
  
        control.limits.lcl = rep(-3,15),
        zone.a.lower = rep(-3,15),
        zone.ab.lower = rep(-2,15),
        zone.bc.lower = rep(-1,15)
    )
  )
})

test_that("Nelson Test 5 3/4 Zone A or beyond (w/create)", {
  expect_equal(
      c(T,T,T,F,F,T,T,T,T,F,F,F,F,F,F)
      ,spc.controlviolation.nelson.1984.test5.zone.a.create(point.count = 3, outof=4)(
        #                  T   T   T F  F T    T  T     T  F    F   F   F    F   F
        chart.series = c(2.5,2.5,2.5,1,-1,1,-2.5,-2.5,-2.5,1,-1.5,1.5,-2.1,-2.1,-1),
  
        center.line = rep(0,15),
  
        control.limits.ucl = rep(3,15),
        zone.a.upper = rep(3,15),
        zone.ab.upper = rep(2,15),
        zone.bc.upper = rep(1,15),
  
        control.limits.lcl = rep(-3,15),
        zone.a.lower = rep(-3,15),
        zone.ab.lower = rep(-2,15),
        zone.bc.lower = rep(-1,15)
    )
  )
})