# -----------------------------------------------------------------------------
# power.variance.twosample.independent
# Same idea with F critical values f.lower / f.upper under the ratio density.
# -----------------------------------------------------------------------------
power.variance.twosample.independent <- function(variance.estimate.g1 = 2,
                                                 variance.estimate.g2 = 1,
                                                 sample.size.g1 = 1,
                                                 sample.size.g2 = 1,
                                                 alpha = 0.05,
                                                 alternative = c("two.sided", "greater", "less"),
                                                 details = TRUE) {
  validate.htest.alternative(alternative = alternative)
  ratio <- variance.estimate.g1 / variance.estimate.g2
  df.g1 <- sample.size.g1 - 1
  df.g2 <- sample.size.g2 - 1
  if (alternative[1] == "two.sided") {
    f.lower <- qf(alpha / 2, df.g1, df.g2, lower.tail = TRUE)
    f.upper <- qf(1 - alpha / 2, df.g1, df.g2, lower.tail = TRUE)
  } else {
    f.lower <- qf(alpha, df.g1, df.g2, lower.tail = TRUE)
    f.upper <- qf(1 - alpha, df.g1, df.g2, lower.tail = TRUE)
  }
  p.fn <- function(q, lower.tail = TRUE) {
    f <- function(x) (1 / ratio) * df(x / ratio, df.g1, df.g2)
    if (lower.tail) {
      rmnames(integrate(f, 0, q)$value)
    } else {
      rmnames(integrate(f, q, Inf)$value)
    }
  }

  if (ratio == 1) {
    beta <- 1
  } else if (alternative[1] == "two.sided") {
    pow <- p.fn(f.lower, lower.tail = TRUE) + p.fn(f.upper, lower.tail = FALSE)
    beta <- 1 - pow
  } else if (ratio < 1) {
    if (alternative[1] == "less") {
      beta <- p.fn(f.lower, lower.tail = FALSE)
    } else {
      beta <- p.fn(f.upper, lower.tail = TRUE)
    }
  } else {
    if (alternative[1] == "less") {
      beta <- p.fn(f.lower, lower.tail = FALSE)
    } else {
      beta <- p.fn(f.upper, lower.tail = TRUE)
    }
  }
  pow <- 1 - beta
  if (details) {
    as.data.frame(list(
      test = "F", type = "two.sample", alternative = alternative[1],
      sample.size.g1 = sample.size.g1, sample.size.g2 = sample.size.g2,
      df.g1 = df.g1, df.g2 = df.g2, ratio = ratio, alpha = alpha,
      conf.level = 1 - alpha, beta = beta, power = pow
    ))
  } else {
    pow
  }
}
