% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spc.constant.calculation.D3.uppercase.R,
%   R/spc.constant.calculation.d3.lowercase.R
\name{spc.constant.calculation.D3}
\alias{spc.constant.calculation.D3}
\alias{spc.constant.calculation.d3}
\title{Control Chart Constant d3 / D3}
\usage{
spc.constant.calculation.D3(sample.size, n.sigma = 3, negative.as.NA = T)

spc.constant.calculation.d3(sample.size)
}
\arguments{
\item{sample.size}{A scalar or vector of sample sizes}

\item{n.sigma}{Scalar - How many standard errors to use (default 3 for most control charts)}

\item{negative.as.NA}{Logical - negative values are possible from the calculation, but not useful. Set to false to obtain negative values instead of NA.}
}
\value{
A scalar or vector with computed constants.
}
\description{
Calculate d3 or D3 control chart constant from Wheeler's SPC books. These are in the same help file 
because of a quirk of how R generates help file names, but they are different constants with different uses.
}
