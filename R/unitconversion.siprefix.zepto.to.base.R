#' Unit Conversion - SI Prefixes - Zepto- to Base 
#'
#' Zepto- means 1/1 000 000 000 000 000 000 000 or 10^(-21) 
#' 
#' Performs a conversion from zepto-units to base units (ex. zeptograms to grams). 
#'
#' @param x Vector - Values in units of zepto-units
#'
#' @return x, but converted to base units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.zepto.to.base <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x / (10^21)
}

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptosecond.to.second <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zs.to.s <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptometer.to.meter <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zm.to.m <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptogram.to.gram <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zg.to.g <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptoampere.to.ampere <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zA.to.A <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptokelvin.to.kelvin <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zK.to.K <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptomole.to.mole <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zmol.to.mol <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptocandela.to.candela <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zcd.to.cd <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptoradian.to.radian <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zrad.to.rad <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptosteradian.to.steradian <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zsr.to.sr <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptohertz.to.hertz <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zHz.to.Hz <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptonewton.to.newton <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zN.to.N <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptopascal.to.pascal <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zPa.to.Pa <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptojoule.to.joule <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zJ.to.J <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptowatt.to.watt <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zW.to.W <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptocoulomb.to.coulomb <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zC.to.C <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptovolt.to.volt <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zV.to.V <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptofarad.to.farad <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zF.to.F <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptoohm.to.ohm <- unitconversion.siprefix.zepto.to.base




#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptosiemens.to.siemens <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zS.to.S <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptoweber.to.weber <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zWb.to.Wb <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptotesla.to.tesla <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zT.to.T <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptohenry.to.henry <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zH.to.H <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptolumen.to.lumen <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zlm.to.lm <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptolux.to.lux <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zlx.to.lx <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptobecquerel.to.becquerel <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zBq.to.Bq <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptogray.to.gray <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zGy.to.Gy <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptosievert.to.sievert <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zSv.to.Sv <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zeptokatal.to.katal <- unitconversion.siprefix.zepto.to.base

#' @rdname unitconversion.siprefix.zepto.to.base
unitconversion.zkat.to.kat <- unitconversion.siprefix.zepto.to.base
