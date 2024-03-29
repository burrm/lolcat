% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/natural.tolerance.R
\name{natural.tolerance}
\alias{natural.tolerance}
\title{Natural Tolerance}
\usage{
natural.tolerance(
  f,
  total.area = 1 - 0.9973,
  upper.tail = total.area/2,
  lower.tail = total.area/2,
  details = T
)
}
\arguments{
\item{f}{Function - f(p, lower.tail) is a quantile function for the probability distribution in question. For example, qnorm.}

\item{total.area}{Scalar/numeric - the total area to use for the calculation, default is .0027}

\item{upper.tail}{Scalar/numeric - upper tail area to use for the calculation, default is .00135}

\item{lower.tail}{Scalar/numeric - lower tail area to use for the calculation, default is .00135}

\item{details}{Scalar/logical - Include calculation details, default is to return details}
}
\value{
Scalar or data.frame - scalar if details = F, otherwise data frame with details from calculation.
}
\description{
Calculates the natural tolerance of a given probability distribution, defined as F(p.upper)-F(p.lower) where 
F is a quantile function for a given univariate distribution. Default is to calculate the natural tolerance 
with a total tail area of .0027, or .00135 on the lower tail and .00135 on the upper tail.
}
