spc.controlviolation.excessvariability.above <- function(x, excess.variation.count = 8, center.line, standard.error, ...) {
  spc.controlviolation.run.above(x = x, excess.variation.count = excess.variation.count, center.line = center.line + standard.error, ...)
}

spc.controlviolation.excessvariability.below <- function(x, excess.variation.count = 8, center.line, standard.error, ...) {
  spc.controlviolation.run.below(x = x, excess.variation.count = excess.variation.count, center.line = center.line - standard.error, ...)
}

spc.controlviolation.excessvariability <- function(x, excess.variation.count = 8, center.line, standard.error, ...) {
  ret.1 <- spc.controlviolation.excessvariability.above(x = x, excess.variation.count = excess.variation.count, center.line = center.line, standard.error = standard.error, ...) 
  ret.2 <- spc.controlviolation.excessvariability.below(x = x, excess.variation.count = excess.variation.count, center.line = center.line, standard.error = standard.error, ...) 
  ret.1 | ret.2
}