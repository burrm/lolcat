#' Absolute Deviation From Median (Midpoint Removed)
#' 
#' Calculates absolute deviation from median (ADM) while removing the midpoint.
#'
#' @param x Vector to compute \eqn{ADM_{n-1}}{ADMn-1} values for.
#'
#' @return Vector with results of \eqn{ADM_{n-1}}{ADMn-1} calculation.
dispersion.ADMn1 <- function(x) {
  x.adm   <- dispersion.ADM(x)
  min.adm <- min(x.adm)
  x.adm[which(x.adm == min.adm)[1]]<-NA
  x.adm
}