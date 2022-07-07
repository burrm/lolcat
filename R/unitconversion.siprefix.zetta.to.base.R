#' Unit Conversion - SI Prefixes - Zetta- to Base  
#' 
#' Zetta- means 1 000 000 000 000 000 000 000 or 10^21 
#'
#' Performs a conversion from zetta-units to base units (ex. zettagrams to grams). 
#'
#' @param x Vector - Values in units of zetta-units
#'
#' @return x, but converted to base units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.zetta.to.base <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x * (10^21)
}

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettasecond.to.second <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zs.to.s <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettameter.to.meter <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zm.to.m <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettagram.to.gram <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zg.to.g <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettaampere.to.ampere <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZA.to.A <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettakelvin.to.kelvin <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZK.to.K <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettamole.to.mole <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zmol.to.mol <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettacandela.to.candela <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zcd.to.cd <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettaradian.to.radian <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zrad.to.rad <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettasteradian.to.steradian <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zsr.to.sr <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettahertz.to.hertz <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZHz.to.Hz <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettanewton.to.newton <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZN.to.N <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettapascal.to.pascal <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZPa.to.Pa <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettajoule.to.joule <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZJ.to.J <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettawatt.to.watt <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZW.to.W <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettacoulomb.to.coulomb <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZC.to.C <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettavolt.to.volt <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZV.to.V <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettafarad.to.farad <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZF.to.F <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettaohm.to.ohm <- unitconversion.siprefix.zetta.to.base




#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettasiemens.to.siemens <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZS.to.S <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettaweber.to.weber <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZWb.to.Wb <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettatesla.to.tesla <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZT.to.T <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettahenry.to.henry <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZH.to.H <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettalumen.to.lumen <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zlm.to.lm <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettalux.to.lux <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zlx.to.lx <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettabecquerel.to.becquerel <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZBq.to.Bq <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettagray.to.gray <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZGy.to.Gy <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettasievert.to.sievert <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.ZSv.to.Sv <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.zettakatal.to.katal <- unitconversion.siprefix.zetta.to.base

#' @rdname unitconversion.siprefix.zetta.to.base
unitconversion.Zkat.to.kat <- unitconversion.siprefix.zetta.to.base
