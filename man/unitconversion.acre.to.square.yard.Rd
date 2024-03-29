% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unitconversion.acre.to.square.yard.R
\name{unitconversion.acre.to.square.yard}
\alias{unitconversion.acre.to.square.yard}
\alias{unitconversion.ac.to.sq.yd}
\title{Unit Conversion - Area - Acre to Square Yard}
\usage{
unitconversion.acre.to.square.yard(x = 1)

unitconversion.ac.to.sq.yd(x = 1)
}
\arguments{
\item{x}{Vector - Values in units of acres}
}
\value{
x, but converted to square yards
}
\description{
Performs a conversion of areas from acres to square yards. Note, per NIST standard, this is a "survey foot"
instead of an "international foot" as is commonly used. In 2022, this will likely become the "international foot."
For most purposes, either survey feet or international feet may be used, but very large areas being calculated will have
an error of roughly a quarter sheet of paper (A4 or letter) per acre (2/1 000 000 difference between survey feet 
and international feet).
}
\references{
NIST. Handbook 133 - Checking the Net Contents of Packaged Goods - Appendix E -  General Tables of Units of Measurement. 2020.  
https://www.nist.gov/system/files/documents/2019/12/03/00-20-h133-apdxE_final-17.pdf
}
