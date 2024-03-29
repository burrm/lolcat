% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute.group.dispersion.ADMn1.R
\name{compute.group.dispersion.ADMn1}
\alias{compute.group.dispersion.ADMn1}
\title{Calculate Subgroup Dispersion - Median (midpoint removed) Absolute Deviation}
\usage{
compute.group.dispersion.ADMn1(fx, data = NULL)
}
\arguments{
\item{fx}{Formula - defines formula for dependent variable and subgroups ( dv ~ v1 + v2 + ...)}

\item{data}{Data Frame - data frame with columns corresponding to variables in fx.}
}
\value{
A vector containing absolute deviations from subgroup medians (with midpoint removed).
}
\description{
Calculate subgroup dispersion using absolute deviation from subgroup median (with midpoint removed).
}
