msa.postprocess.ev.number.discrimination.ratio.simple <- function(
  component.variance.total,
  component.variance.gage.r.r
) {
  sqrt((2*component.variance.total/component.variance.gage.r.r) - 1)  
}