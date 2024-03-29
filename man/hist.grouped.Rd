% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.grouped.R
\name{hist.grouped}
\alias{hist.grouped}
\title{Grouped Histogram}
\usage{
\method{hist}{grouped}(
  x,
  interval.size = NA,
  width.consider = lolcat.default.width.consider,
  anchor.value = NA,
  col = "lightblue",
  main = "Grouped Histogram",
  right = F,
  include.lowest = T,
  stat.lsl = NA,
  stat.lsl.label = "LSL",
  stat.target = NA,
  stat.target.label = "TGT",
  stat.usl = NA,
  stat.usl.label = "USL",
  after.plot = function(x, ...) {
 },
  freq = T,
  xlim = c(min(na.omit(c(x, stat.lsl, anchor.value))), max(na.omit(c(x, stat.usl,
    anchor.value)))),
  ...
)
}
\arguments{
\item{x}{Vector/numeric - The data for the plot}

\item{interval.size}{Numeric - Bin size}

\item{width.consider}{Numeric - Bin sizes to consider when plotting histograms}

\item{anchor.value}{Numeric - One of the breakpoints}

\item{col}{Character - color of bars}

\item{main}{Character - Main chart title}

\item{right}{Logical - If TRUE, the histogram cells are right-closed (left open) intervals.}

\item{include.lowest}{Logical - If TRUE, an x[i] equal to the breaks value will be included in the first (or last, for right = FALSE) bar. This will be ignored (with a warning) unless breaks is a vector.}

\item{stat.lsl}{Numeric - Lower specification limit}

\item{stat.lsl.label}{Character - Lower specification limit label}

\item{stat.target}{Numeric - Process target}

\item{stat.target.label}{Character - Process target label}

\item{stat.usl}{Numeric - Upper specification limit}

\item{stat.usl.label}{Character - Upper specification limit label}

\item{after.plot}{Function - Function to perform after plotting, see hist.add.distribution.curve.normal}

\item{freq}{Logical - If TRUE, use frequency instead of probability densities.}

\item{xlim}{Numeric - Chart Domain, expressed as c(x.min,x.max)}

\item{...}{Additional parameters - Additional parameters passed to hist() and after.plot()}
}
\value{
Return value of hist()
}
\description{
Prepares a grouped histogram of a particular set of data.
}
