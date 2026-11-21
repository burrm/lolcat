#' Unit Conversion - SI Prefixes - Yocto- to Base 
#'
#' Yocto- means 1/1 000 000 000 000 000 000 000 000 or 10^(-24) 
#' 
#' Performs a conversion from yocto-units to base units (ex. yoctograms to grams). 
#'
#' @param x Vector - Values in units of yocto-units
#'
#' @return x, but converted to base units 
#'
#' @references
#' NIST. Metric (SI) Prefixes. 2022. Accessed 4/7/2022. 
#' https://www.nist.gov/pml/weights-and-measures/metric-si-prefixes
unitconversion.siprefix.yocto.to.base <- function(
  x = 1
) {
  warning("Although this function is provided, you may want to use an arbitrary precision library to avoid floating point issues.")
  x / (10^24)
}

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctosecond.to.second <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ys.to.s <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctometer.to.meter <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ym.to.m <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctogram.to.gram <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yg.to.g <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctoampere.to.ampere <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yA.to.A <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctokelvin.to.kelvin <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yK.to.K <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctomole.to.mole <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ymol.to.mol <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctocandela.to.candela <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ycd.to.cd <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctoradian.to.radian <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yrad.to.rad <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctosteradian.to.steradian <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ysr.to.sr <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctohertz.to.hertz <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yHz.to.Hz <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctonewton.to.newton <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yN.to.N <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctopascal.to.pascal <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yPa.to.Pa <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctojoule.to.joule <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yJ.to.J <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctowatt.to.watt <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yW.to.W <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctocoulomb.to.coulomb <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yC.to.C <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctovolt.to.volt <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yV.to.V <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctofarad.to.farad <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yF.to.F <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctoohm.to.ohm <- unitconversion.siprefix.yocto.to.base




#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctosiemens.to.siemens <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yS.to.S <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctoweber.to.weber <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yWb.to.Wb <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctotesla.to.tesla <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yT.to.T <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctohenry.to.henry <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yH.to.H <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctolumen.to.lumen <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ylm.to.lm <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctolux.to.lux <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ylx.to.lx <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctobecquerel.to.becquerel <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yBq.to.Bq <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctogray.to.gray <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yGy.to.Gy <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctosievert.to.sievert <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ySv.to.Sv <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.yoctokatal.to.katal <- unitconversion.siprefix.yocto.to.base

#' @rdname unitconversion.siprefix.yocto.to.base
unitconversion.ykat.to.kat <- unitconversion.siprefix.yocto.to.base
