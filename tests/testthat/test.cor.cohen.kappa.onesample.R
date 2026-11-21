require(lolcat)

context("Correlation/Association - Cohen's Kappa - One Sample")

test_that("Cohen (1960) - Table 1 Multiplied by 100", {
  m <- matrix(c(25,13,12,12,2,16,3,15,2), byrow = T, nrow = 3)
  exp.freq <- chi.square.2d.expected.frequencies(m)
  
  expect_equal(20, round(exp.freq[1,1],0))
  expect_equal(15, round(exp.freq[1,2],0))
  expect_equal(15, round(exp.freq[1,3],0))
  
  expect_equal(12, round(exp.freq[2,1],0))
  expect_equal(9, round(exp.freq[2,2],0))
  expect_equal(9, round(exp.freq[2,3],0))
  
  expect_equal(8, round(exp.freq[3,1],0))
  expect_equal(6, round(exp.freq[3,2],0))
  expect_equal(6, round(exp.freq[3,3],0))
  
  #cor.cohen.kappa.onesample(m)
})

test_that("Cohen (1960) - Table 2 - Frequencies", {
  m <- matrix(c(88,14,18,10,40,10,2,6,12), byrow = T, nrow = 3)
  exp.freq <- chi.square.2d.expected.frequencies(m)
  
  expect_equal(60, round(exp.freq[1,1],0))
  #expect_equal(14, round(exp.freq[1,2],0))
  #expect_equal(18, round(exp.freq[1,3],0))
  
  #expect_equal(10, round(exp.freq[2,1],0))
  expect_equal(18, round(exp.freq[2,2],0))
  #expect_equal(10, round(exp.freq[2,3],0))
  
  #expect_equal(2, round(exp.freq[3,1],0))
  #expect_equal(6, round(exp.freq[3,2],0))
  expect_equal(4, round(exp.freq[3,3],0))
  
  t.out <- cor.cohen.kappa.onesample.1960.cohen(m)
  
  expect_equal(c(z=8.34), round(t.out$statistic,2))
  expect_equal(c(kappa=.492), round(t.out$estimate[1],3))
  expect_equal(c(se.kappa=.055), round(t.out$estimate[2],3))

})


test_that("Fleiss (1971) - Table 1 - Kappa and SE", {
  rn <- c("Depression", "Personality", "Schizophrenia", "Neurosis", "Other")
  subject <- rep(1:30,each=6)
  rating <- c(
    #1
    rep(rn[4],6),
    rep(rn[2],3),rep(rn[5],3),
    rep(rn[2],1),rep(rn[3],4),rep(rn[5],1),
    rep(rn[5],6),
    rep(rn[2],3),rep(rn[4],3),
    #6
    rep(rn[1],2),rep(rn[3],4),
    rep(rn[3],4),rep(rn[5],2),
    rep(rn[1],2),rep(rn[3],3),rep(rn[4],1),
    rep(rn[1],2),rep(rn[4],4),
    rep(rn[5],6),
    #11
    rep(rn[1],1),rep(rn[4],5),
    rep(rn[1],1),rep(rn[2],1),rep(rn[4],4),
    rep(rn[2],3),rep(rn[3],3),
    rep(rn[1],1),rep(rn[4],5),
    rep(rn[2],2),rep(rn[4],3),rep(rn[5],1),
    #16
    rep(rn[3],5),rep(rn[5],1),
    rep(rn[1],3),rep(rn[4],1),rep(rn[5],2),
    rep(rn[1],5),rep(rn[2],1),
    rep(rn[2],2),rep(rn[4],4),
    rep(rn[1],1),rep(rn[3],2),rep(rn[5],3),
    #21
    rep(rn[5],6),
    rep(rn[2],1),rep(rn[4],5),
    rep(rn[2],2),rep(rn[4],1),rep(rn[5],3),
    rep(rn[1],2),rep(rn[4],4),
    rep(rn[1],1),rep(rn[4],4),rep(rn[5],1),
    #26
    rep(rn[2],5),rep(rn[4],1),
    rep(rn[1],4),rep(rn[5],2),
    rep(rn[2],2),rep(rn[4],4),
    rep(rn[1],1),rep(rn[3],5),
    rep(rn[5],6)
  )
  
  t.out <- cor.cohen.kappa.onesample.1971.fleiss(subject = subject, rating = rating)
  
  expect_equal(c(z=15.6), round(t.out$statistic,1))
  expect_equal(c(kappa=.430), round(t.out$estimate[1],3))
  expect_equal(c(se.kappa=.028), round(t.out$estimate[2],3))
  
})
