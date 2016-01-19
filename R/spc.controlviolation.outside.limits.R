spc.controlviolation.outside.ucl <- function(x, upper.control.limit = NA, ...) {
  x > ucl
}

spc.controlviolation.outside.lcl <- function(x, lower.control.limit = NA, ...) {
  x < lcl
}

spc.controlviolation.outside.limits <- function(x, lower.control.limit = NA, upper.control.limit = NA, ...) {
  tmp.ucl <- spc.controlviolation.outside.ucl(x = x, upper.control.limit = upper.control.limit, ...)
  tmp.lcl <- spc.controlviolation.outside.lcl(x = x, lower.control.limit = lower.control.limit, ...)
  
  if (anyNA(tmp.ucl)) {tmp.ucl[which(is.na(tmp.ucl))] <- F}
  if (anyNA(tmp.lcl)) {tmp.lcl[which(is.na(tmp.lcl))] <- F}
  
  tmp.ucl | tmp.lcl
}