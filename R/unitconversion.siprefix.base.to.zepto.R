#' Unit Conversion - SI Prefixes - Base to Zepto-  
#'
#' Zepto- means 1/1 000 000 000 000 000 000 000 or 10^(-21) 
#' 
#' Performs a conversion from base units to zepto-units (ex. grams to zeptograms). 
#'
#' @param x Vector - Values in units of base units
#'
#' @return x, but converted to zepto-units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.base.to.zepto <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x * (10^21)
}

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.second.to.zeptosecond <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.s.to.zs <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.meter.to.zeptometer <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.m.to.zm <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.gram.to.zeptogram <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.g.to.zg <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.ampere.to.zeptoampere <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.A.to.zA <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.kelvin.to.zeptokelvin <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.K.to.zK <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.mole.to.zeptomole <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.mol.to.zmol <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.candela.to.zeptocandela <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.cd.to.zcd <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.radian.to.zeptoradian <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.rad.to.zrad <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.steradian.to.zeptosteradian <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.sr.to.zsr <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.hertz.to.zeptohertz <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.Hz.to.zHz <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.newton.to.zeptonewton <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.N.to.zN <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.pascal.to.zeptopascal <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.Pa.to.zPa <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.joule.to.zeptojoule <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.J.to.zJ <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.watt.to.zeptowatt <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.W.to.zW <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.coulomb.to.zeptocoulomb <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.C.to.zC <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.volt.to.zeptovolt <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.V.to.zV <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.farad.to.zeptofarad <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.F.to.zF <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.ohm.to.zeptoohm <- unitconversion.siprefix.base.to.zepto




#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.siemens.to.zeptosiemens <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.S.to.zS <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.weber.to.zeptoweber <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.Wb.to.zWb <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.tesla.to.zeptotesla <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.T.to.zT <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.henry.to.zeptohenry <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.H.to.zH <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.lumen.to.zeptolumen <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.lm.to.zlm <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.lux.to.zeptolux <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.lx.to.zlx <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.becquerel.to.zeptobecquerel <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.Bq.to.zBq <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.gray.to.zeptogray <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.Gy.to.zGy <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.sievert.to.zeptosievert <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.Sv.to.zSv <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.katal.to.zeptokatal <- unitconversion.siprefix.base.to.zepto

#' @rdname unitconversion.siprefix.base.to.zepto
unitconversion.kat.to.zkat <- unitconversion.siprefix.base.to.zepto
