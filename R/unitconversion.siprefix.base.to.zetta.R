#' Unit Conversion - SI Prefixes - Base to Zetta-  
#' 
#' Zetta- means 1 000 000 000 000 000 000 000 or 10^21 
#'
#' Performs a conversion from base units to zetta-units (ex. grams to zettagrams). 
#'
#' @param x Vector - Values in units of base units
#'
#' @return x, but converted to zetta-units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.base.to.zetta <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x / (10^21)
}

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.second.to.zettasecond <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.s.to.Zs <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.meter.to.zettameter <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.m.to.Zm <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.gram.to.zettagram <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.g.to.Zg <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.ampere.to.zettaampere <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.A.to.ZA <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.kelvin.to.zettakelvin <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.K.to.ZK <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.mole.to.zettamole <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.mol.to.Zmol <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.candela.to.zettacandela <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.cd.to.Zcd <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.radian.to.zettaradian <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.rad.to.Zrad <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.steradian.to.zettasteradian <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.sr.to.Zsr <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.hertz.to.zettahertz <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.Hz.to.ZHz <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.newton.to.zettanewton <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.N.to.ZN <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.pascal.to.zettapascal <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.Pa.to.ZPa <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.joule.to.zettajoule <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.J.to.ZJ <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.watt.to.zettawatt <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.W.to.ZW <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.coulomb.to.zettacoulomb <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.C.to.ZC <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.volt.to.zettavolt <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.V.to.ZV <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.farad.to.zettafarad <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.F.to.ZF <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.ohm.to.zettaohm <- unitconversion.siprefix.base.to.zetta




#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.siemens.to.zettasiemens <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.S.to.ZS <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.weber.to.zettaweber <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.Wb.to.ZWb <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.tesla.to.zettatesla <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.T.to.ZT <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.henry.to.zettahenry <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.H.to.ZH <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.lumen.to.zettalumen <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.lm.to.Zlm <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.lux.to.zettalux <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.lx.to.Zlx <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.becquerel.to.zettabecquerel <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.Bq.to.ZBq <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.gray.to.zettagray <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.Gy.to.ZGy <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.sievert.to.zettasievert <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.Sv.to.ZSv <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.katal.to.zettakatal <- unitconversion.siprefix.base.to.zetta

#' @rdname unitconversion.siprefix.base.to.zetta
unitconversion.kat.to.Zkat <- unitconversion.siprefix.base.to.zetta
