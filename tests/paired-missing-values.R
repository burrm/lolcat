library(lolcat)

local({
  assert.equal <- function(actual, expected) {
    stopifnot(isTRUE(all.equal(actual, expected)))
  }

  # Staggered missing values must remove whole pairs, even when the number
  # of missing values differs between the two vectors.
  for (g2 in list(c(2, 5, NA, 6, 2, 9, 3, 7),
                  c(2, 5, NA, 6, NA, 9, 3, 7))) {
    g1 <- c(1, NA, 3, 4, 6, 8, 5, 2)
    clean <- na.omit(data.frame(g1 = g1, g2 = g2))
    for (fun in list(median.test.twosample.dependent.signtest,
                     median.test.twosample.dependent.wilcoxon,
                     wilcoxon.signed.ranks.twosample.test)) {
      assert.equal(fun(g1, g2), fun(clean$g1, clean$g2))
    }

    for (normality in c("yes", "no")) {
      for (method in c("meandiff", "dbar")) {
        assert.equal(
          variance.test.twosample.dependent(g1, g2,
            assume.normality = normality, non.norm.method = method),
          variance.test.twosample.dependent(clean$g1, clean$g2,
            assume.normality = normality, non.norm.method = method))
      }
    }
  }

  # The email's example has only two complete pairs.
  g1 <- c(1, NA, 3, 4)
  g2 <- c(2, 5, NA, 6)
  assert.equal(median.test.twosample.dependent.signtest(g1, g2),
               median.test.twosample.dependent.signtest(c(1, 4), c(2, 6)))
  assert.equal(wilcoxon.signed.ranks.twosample.test(g1, g2),
               wilcoxon.signed.ranks.twosample.test(c(1, 4), c(2, 6)))

  x1 <- c(1, NA, 3, 4, 6, 8, 5, 2, 7, 9)
  x2 <- c(2, 5, NA, 6, 2, 9, 3, 7, 8, 1)
  x3 <- c(3, 2, 7, NaN, 4, 6, 1, 9, 5, 8)
  clean <- na.omit(data.frame(x1, x2, x3))
  for (method in c("Steiger", "Hotelling")) {
    assert.equal(cor.pearson.r.twosample.dependent(x1, x2, x3, method = method),
                 cor.pearson.r.twosample.dependent(
                   clean$x1, clean$x2, clean$x3, method = method))
  }

  # Independent correlation samples need their own complete-case masks.
  x4 <- c(7, 2, NA, 8, 6, 1, 5, 3)
  x3 <- x3[seq_along(x4)]
  first <- na.omit(data.frame(x1, x2))
  second <- na.omit(data.frame(x3, x4))
  assert.equal(cor.pearson.r.twosample.independent(x1, x2, x3, x4),
               cor.pearson.r.twosample.independent(
                 first$x1, first$x2, second$x3, second$x4))

  disc <- factor(c("a", "b", NA, "a", "b", "a", "b", "a", "b", "a"))
  cont <- c(1, 5, 100, NA, 6, 3, 8, 2, 4, 7)
  clean <- na.omit(data.frame(disc, cont))
  for (fun in list(cor.biserial, cor.point.biserial)) {
    assert.equal(fun(disc, cont), fun(clean$disc, clean$cont))
    assert.equal(fun(as.character(disc), cont), fun(clean$disc, clean$cont))
  }

  rows <- factor(c("a", NA, "a", "b", "b", "a"))
  cols <- c("x", "x", NA, "y", "x", "y")
  weights <- c(2, 100, 200, 7, NA, 3)
  expected <- matrix(c(2, 0, 3, 7), nrow = 2,
                     dimnames = list(row_label = c("a", "b"),
                                     col_label = c("x", "y")))
  assert.equal(transform.independent.format.to.xt(rows, cols, weights), expected)
  assert.equal(transform.individual.format.to.xt(rows, cols, weights), expected)
  assert.equal(transform.independent.format.to.xt(rows, cols),
               transform.independent.format.to.xt(rows, cols, rep(1, length(rows))))
  # Disabling omission should preserve missing weights in the result.
  stopifnot(is.na(transform.independent.format.to.xt(
    "a", "x", NA_real_, na.rm = FALSE)[1, 1]))
  empty <- transform.independent.format.to.xt(NA_character_, "x", 1)
  stopifnot(identical(dim(empty), c(0L, 0L)))
})
