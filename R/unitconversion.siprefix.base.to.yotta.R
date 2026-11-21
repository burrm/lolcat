#' Unit Conversion - SI Prefixes - Base to Yotta-  
#' 
#' Yotta- means 1 000 000 000 000 000 000 000 000 or 10^24 
#'
#' Performs a conversion from base units to yotta-units (ex. grams to yottagrams). 
#'
#' @param x Vector - Values in units of base units
#'
#' @return x, but converted to yotta-units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.base.to.yotta <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x / (10^24)
}

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.second.to.yottasecond <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.s.to.Ys <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.meter.to.yottameter <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.m.to.Ym <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.gram.to.yottagram <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.g.to.Yg <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.ampere.to.yottaampere <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.A.to.YA <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.kelvin.to.yottakelvin <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.K.to.YK <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.mole.to.yottamole <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.mol.to.Ymol <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.candela.to.yottacandela <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.cd.to.Ycd <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.radian.to.yottaradian <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.rad.to.Yrad <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.steradian.to.yottasteradian <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.sr.to.Ysr <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.hertz.to.yottahertz <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.Hz.to.YHz <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.newton.to.yottanewton <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.N.to.YN <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.pascal.to.yottapascal <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.Pa.to.YPa <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.joule.to.yottajoule <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.J.to.YJ <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.watt.to.yottawatt <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.W.to.YW <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.coulomb.to.yottacoulomb <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.C.to.YC <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.volt.to.yottavolt <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.V.to.YV <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.farad.to.yottafarad <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.F.to.YF <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.ohm.to.yottaohm <- unitconversion.siprefix.base.to.yotta




#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.siemens.to.yottasiemens <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.S.to.YS <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.weber.to.yottaweber <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.Wb.to.YWb <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.tesla.to.yottatesla <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.T.to.YT <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.henry.to.yottahenry <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.H.to.YH <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.lumen.to.yottalumen <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.lm.to.Ylm <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.lux.to.yottalux <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.lx.to.Ylx <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.becquerel.to.yottabecquerel <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.Bq.to.YBq <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.gray.to.yottagray <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.Gy.to.YGy <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.sievert.to.yottasievert <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.Sv.to.YSv <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.katal.to.yottakatal <- unitconversion.siprefix.base.to.yotta

#' @rdname unitconversion.siprefix.base.to.yotta
unitconversion.kat.to.Ykat <- unitconversion.siprefix.base.to.yotta
