#' Unit Conversion - SI Prefixes - Base to Yocto-  
#'
#' Yocto- means 1/1 000 000 000 000 000 000 000 000 or 10^(-24) 
#' 
#' Performs a conversion from base units to yocto-units (ex. grams to yoctograms). 
#'
#' @param x Vector - Values in units of base units
#'
#' @return x, but converted to yocto-units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.base.to.yocto <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x * (10^24)
}

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.second.to.yoctosecond <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.s.to.ys <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.meter.to.yoctometer <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.m.to.ym <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.gram.to.yoctogram <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.g.to.yg <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.ampere.to.yoctoampere <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.A.to.yA <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.kelvin.to.yoctokelvin <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.K.to.yK <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.mole.to.yoctomole <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.mol.to.ymol <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.candela.to.yoctocandela <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.cd.to.ycd <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.radian.to.yoctoradian <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.rad.to.yrad <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.steradian.to.yoctosteradian <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.sr.to.ysr <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.hertz.to.yoctohertz <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.Hz.to.yHz <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.newton.to.yoctonewton <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.N.to.yN <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.pascal.to.yoctopascal <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.Pa.to.yPa <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.joule.to.yoctojoule <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.J.to.yJ <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.watt.to.yoctowatt <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.W.to.yW <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.coulomb.to.yoctocoulomb <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.C.to.yC <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.volt.to.yoctovolt <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.V.to.yV <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.farad.to.yoctofarad <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.F.to.yF <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.ohm.to.yoctoohm <- unitconversion.siprefix.base.to.yocto




#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.siemens.to.yoctosiemens <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.S.to.yS <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.weber.to.yoctoweber <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.Wb.to.yWb <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.tesla.to.yoctotesla <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.T.to.yT <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.henry.to.yoctohenry <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.H.to.yH <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.lumen.to.yoctolumen <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.lm.to.ylm <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.lux.to.yoctolux <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.lx.to.ylx <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.becquerel.to.yoctobecquerel <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.Bq.to.yBq <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.gray.to.yoctogray <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.Gy.to.yGy <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.sievert.to.yoctosievert <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.Sv.to.ySv <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.katal.to.yoctokatal <- unitconversion.siprefix.base.to.yocto

#' @rdname unitconversion.siprefix.base.to.yocto
unitconversion.kat.to.ykat <- unitconversion.siprefix.base.to.yocto
