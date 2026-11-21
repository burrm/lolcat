msa.postprocess.ev.number.distinct.categories <- function(
  model.ev
) {
  #rows
  gage_tot_rr               <- 1
  variability_part_part     <- 6

  #cols
  study_variation     <- 4

  msa.postprocess.ev.number.distinct.categories.simple(
    study.variation.part.part = model.ev[variability_part_part, study_variation],
    study.variation.total     = model.ev[gage_tot_rr,           study_variation]
  )
}