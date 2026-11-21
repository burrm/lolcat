require(lolcat)

context("Check proportion sample size (MOE) adaptation to Wendy's original")

test_that("Package version matches two.sided version", {
  
  find_n_prop_beta_two_sided <- function(alpha, P, ME, max_n = 10000) {
    for (n in 1:max_n) {
      # conservative counts for each tail
      x_lo <- floor(n * P)
      x_hi <- ceiling(n * P)
      
      # lower limit with x_lo
      PL <- if (x_lo == 0) {
        0
      } else {
        qbeta(alpha/2, x_lo,      n - x_lo + 1)
      }
      
      # upper limit with x_hi
      PU <- if (x_hi >= n) {
        1
      } else {
        qbeta(1 - alpha/2, x_hi + 1, n - x_hi)
      }
      
      # check both margins
      if ((P - PL) <= ME && (PU - P) <= ME) {
        return(n)
      }
    }
    warning("No n ≤ ", max_n, " meets the two-sided exact CI width requirement")
    NA
  }

  test_input <- expand.grid(
    proportion=c(.25,.5,.75), 
    margin.of.error = c(.1,.2,.3), 
    alpha = c(.05,.1,.2)
  )
  
  #Test assertions
  for (i in 1:nrow(test_input)) {
    p <- test_input$proportion[i]
    m <- test_input$margin.of.error[i]
    a <- test_input$alpha[i]

    expect_equal(
      sample.size.proportion.test.onesample.moe.approximate(proportion = p, margin.of.error = m, alpha = a, details = F),
      find_n_prop_beta_two_sided(alpha = a, P = p, ME = m)
      
    )
  }


})

test_that("Package version matches less version", {
  find_n_prop_beta_lower <- function(alpha, P, ME, max_n = 10000) {
    for (n in 1:max_n) {
      x <- floor(n * P)  
      PL <- if (x == 0) {
        0
      } else {
        qbeta(alpha, x, n - x + 1)
      }
      if ((P - PL) <= ME) {
        return(n)
      }
    }
    warning("No n ≤ ", max_n, " meets one-sided lower requirement")
    NA
  }

  test_input <- expand.grid(
    proportion=c(.25,.5,.75), 
    margin.of.error = c(.1,.2,.3), 
    alpha = c(.05,.1,.2)
  )
  
  #Test assertions
  for (i in 1:nrow(test_input)) {
    p <- test_input$proportion[i]
    m <- test_input$margin.of.error[i]
    a <- test_input$alpha[i]

    expect_equal(
      sample.size.proportion.test.onesample.moe.approximate(proportion = p, margin.of.error = m, alpha = a, alternative="less", details = F),
      find_n_prop_beta_lower(alpha = a, P = p, ME = m)
      
    )
  }

})

test_that("Package version matches greater version", {
  find_n_prop_beta_upper <- function(alpha, P, ME, max_n = 10000) {
    for (n in 1:max_n) {
      x  <- ceiling(n * P)
      PU <- if (x == n) {
        1
      } else {
        qbeta(1 - alpha, x + 1, n - x)
      }
      if ((PU - P) <= ME) {
        return(n)
      }
    }
    warning("No n ≤ ", max_n, " meets one-sided upper requirement")
    NA
  }

  test_input <- expand.grid(
    proportion=c(.25,.5,.75), 
    margin.of.error = c(.1,.2,.3), 
    alpha = c(.05,.1,.2)
  )
  
  #Test assertions
  for (i in 1:nrow(test_input)) {
    p <- test_input$proportion[i]
    m <- test_input$margin.of.error[i]
    a <- test_input$alpha[i]

    expect_equal(
      sample.size.proportion.test.onesample.moe.approximate(proportion = p, margin.of.error = m, alpha = a, alternative="greater", details = F),
      find_n_prop_beta_upper(alpha = a, P = p, ME = m)
      
    )
  }
})

