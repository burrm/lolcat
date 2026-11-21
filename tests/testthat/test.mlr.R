require(lolcat)

context("5620 MLR Tests")

test_that("Mallow's Cp (simple) calculation produces expected output", {
  expect_equal(round(  mlr.mallow.cp.simple(r.squared.model = .651
                                            ,parameter.count.model = 2
                                            ,r.squared.all.vars = .656
                                            ,parameter.count.all.vars = 5
                                            ,sample.size = 51
                                            
  ),3), 1.669)


})
