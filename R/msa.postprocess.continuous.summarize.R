msa.postprocess.continuous.summarize <- function(
  model.aov
  ,model.ev
) {
  ret <- matrix(nrow = 7, ncol = 7)

  #rows
  gage_tot_rr               <- 1
  repeatability             <- 2
  reproducibility_tot       <- 3
  reproducibility_appraiser <- 4
  reproducibility_appr_part <- 5
  variability_part_part     <- 6
  variability_tot           <- 7

  #cols
  calculated_value    <- 1
  contribution_pct    <- 2
  standard_dev        <- 3
  study_variation     <- 4
  study_variation_pct <- 5
  tolerance_pct       <- 6

  aov.col.df      <- 1
  aov.col.sum.sq  <- 2
  aov.col.mean.sq <- 3
  aov.col.f.val   <- 4
  aov.col.f.p     <- 5

  aov.tbl <- model.aov[[1]]

  colnames(ret) <- c(
    "AOV.F", 
    "AOV.P", 
    "Component", 
    "Component_Pct", 
    "StudyVar", 
    "StudyVar_Pct", 
    "Tolerance_Pct"
  )

  rownames(ret) <- c(
    "Total Gage R&R", 
    "  Repeatability", 
    "  Reproducibility", 
    "    Appraiser",
    "Part:Appraiser", 
    "Part-To-Part",
    "Total Variation"
  )

  aov.row.part <- 1
  if (nrow(aov.tbl) == 3) {
    #single appraiser
    aov.row.repeatability <- 2
    
    ret[6,1] <- aov.tbl[aov.row.part, aov.col.f.val]
    ret[6,2] <- aov.tbl[aov.row.part, aov.col.f.p]

  } else if (nrow(aov.tbl) == 4) {
    #2+ reduced model (no interaction)
    aov.row.appraiser     <- 2
    aov.row.repeatability <- 3

    ret[6,1] <- aov.tbl[aov.row.part, aov.col.f.val]
    ret[6,2] <- aov.tbl[aov.row.part, aov.col.f.p]

    ret[4,1] <- aov.tbl[aov.row.appraiser, aov.col.f.val]
    ret[4,2] <- aov.tbl[aov.row.appraiser, aov.col.f.p]

  } else if (nrow(aov.tbl) == 5) {
    #2+ full model (with interaction)
    aov.row.appraiser      <- 2
    aov.row.appraiser.part <- 3
    aov.row.repeatability  <- 4

    ret[6,1] <- aov.tbl[aov.row.part, aov.col.f.val]
    ret[6,2] <- aov.tbl[aov.row.part, aov.col.f.p]

    ret[4,1] <- aov.tbl[aov.row.appraiser, aov.col.f.val]
    ret[4,2] <- aov.tbl[aov.row.appraiser, aov.col.f.p]

    ret[5,1] <- aov.tbl[aov.row.appraiser.part, aov.col.f.val]
    ret[5,2] <- aov.tbl[aov.row.appraiser.part, aov.col.f.p]


  } else {
    stop("different postprocessed aov table than expected")
  }

  ret[,3] <- model.ev[,calculated_value]
  ret[,4] <- model.ev[,contribution_pct]
  ret[,5] <- model.ev[,study_variation]
  ret[,6] <- model.ev[,study_variation_pct]
  ret[,7] <- model.ev[,tolerance_pct]

  ret
}