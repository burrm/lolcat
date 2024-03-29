% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor.pearson.r.twosample.dependent.R,
%   R/cor.pearson.r.twosample.dependent.simple.R
\name{cor.pearson.r.twosample.dependent}
\alias{cor.pearson.r.twosample.dependent}
\alias{cor.pearson.r.twosample.dependent.simple}
\title{Two Dependent Sample Test of Pearson's Correlation Coefficient}
\usage{
cor.pearson.r.twosample.dependent(
  x1,
  x2,
  x3,
  alternative = c("two.sided", "less", "greater"),
  method = c("Steiger", "Hotelling"),
  conf.level = 0.95
)

cor.pearson.r.twosample.dependent.simple(
  sample.r.g1.g3,
  sample.r.g2.g3,
  sample.r.g1.g2,
  sample.size,
  alternative = c("two.sided", "less", "greater"),
  method = c("Steiger", "Hotelling"),
  conf.level = 0.95
)
}
\arguments{
\item{x1}{Vector - Variable 1 values}

\item{x2}{Vector - Variable 2 values}

\item{x3}{Vector - Variable 3 values}

\item{alternative}{The alternative hypothesis to use for the test computation.}

\item{method}{Scalar - character string - method used in calculation.}

\item{conf.level}{The confidence level for this test, between 0 and 1.}

\item{sample.r.g1.g3}{Scalar - Sample correlation coefficient between x1 and x3.}

\item{sample.r.g2.g3}{Scalar - Sample correlation coefficient between x2 and x3.}

\item{sample.r.g1.g2}{Scalar - Sample correlation coefficient between x1 and x2.}

\item{sample.size}{Scalar - Sample size to use for the calculation.}
}
\value{
Hypothesis test result showing results of test.
}
\description{
Calculate test of significance difference for Pearson's Correlation Coefficient between three samples.
Null hypothesis: No significant difference between correlation coefficient between x1 and x3 vs. 
correlation coefficient between x2 and x3.
Significant result: Low p value indicates that a statistically significant difference 
exists between correlation coefficient between x1 and x3 vs. correlation coefficient between x2 and x3.
}
