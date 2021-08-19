msa.postprocess.ev.number.discrimination.ratio <- function(
 model.ev
) {
  #rows
  gage_tot_rr               <- 1
  variability_tot           <- 7

  #cols
  calculated_value    <- 1
  
  msa.postprocess.ev.number.discrimination.ratio.simple(
    component.variance.total    = model.ev[variability_tot ,calculated_value],
    component.variance.gage.r.r = model.ev[gage_tot_rr     ,calculated_value]
  )   
}