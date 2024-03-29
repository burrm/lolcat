% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/sample.size.count.poisson.onesample.approximate.R
\name{sample.size.count.poisson.onesample.approximate}
\alias{sample.size.count.poisson.onesample.approximate}
\title{Sample Size - Single Sample Poisson Test}
\usage{
sample.size.count.poisson.onesample.approximate(
  lambda.null.hypothesis,
  lambda.alternative.hypothesis,
  alpha = 0.05,
  beta = 0.1,
  alternative = c("two.sided", "less", "greater"),
  details = TRUE,
  power.from.actual = F
)
}
\arguments{
\item{lambda.null.hypothesis}{Scalar - null hypothesis lambda parameter}

\item{lambda.alternative.hypothesis}{Scalar - alternative hypothesis lambda parameter}

\item{alpha}{Scalar - Type I error rate}

\item{beta}{Scalar - Type II error rate}

\item{alternative}{Scalar (character) - alternative hypothesis}

\item{details}{Logical - Return calculation details (default) or return only sample size (details = FALSE)}

\item{power.from.actual}{Logical - If true, return 1-beta, if false, calculate power using calculated sample size.}
}
\value{
A data frame with details about the calculation or a single value with sample size (details = F).
}
\description{
Sample size calculation utilizes the square root transformation (approximate sample size/power).
}
