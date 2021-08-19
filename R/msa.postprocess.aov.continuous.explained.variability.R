msa.postprocess.aov.continuous.explained.variability <- function(
  model.aov
  ,stat.lsl = NA
  ,stat.usl = NA
  ,n.sigma  = 6
) {
  ev <- matrix(nrow = 7, ncol = 6)

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
 
  #print("ev model.aov")
  #print(model.aov)

  aov.tbl <- model.aov[[1]]

  colnames(ev) <- c(
    "Component", 
    "Pct_Cont", 
    "StdDev",
    "StudyVar", 
    "Pct_StudyVar", 
    "Pct_Tolerance"
  )

  rownames(ev) <- c(
    "Total Gage R&R", 
    "  Repeatability", 
    "  Reproducibility", 
    "    Appraiser",
    "Part:Appraiser", 
    "Part-To-Part",
    "Total Variation"
  )

  if (nrow(aov.tbl) == 3) {
    #single appraiser
    n.part      <- aov.tbl$Df[1]+1
    n.appraiser <- 1
    n.within    <- sum(aov.tbl$Df,1)/n.part

    ev[repeatability,         calculated_value] <- aov.tbl[2, 3] # repeatability
    ev[variability_part_part, calculated_value] <- max(c((aov.tbl[1, 3] - aov.tbl[2, 3])/(n.appraiser * n.within),0)) # part to part
    ev[gage_tot_rr,           calculated_value] <- ev[repeatability, calculated_value]  # Total GRR = repeatability since no reprod without multiple appraisers
    ev[variability_tot,       calculated_value] <- ev[gage_tot_rr, calculated_value] + ev[variability_part_part, calculated_value]  # Total Variation

  } else if (nrow(aov.tbl) == 4) {
    #2+ reduced model (no interaction)
    n.part      <- aov.tbl$Df[1]+1
    n.appraiser <- aov.tbl$Df[2]+1
    n.within    <- sum(aov.tbl$Df,1)/(n.part*n.appraiser)

    ev[repeatability,             calculated_value] <- aov.tbl[3, 3] ## Repeatability: MS Repeatability
    ev[reproducibility_appraiser, calculated_value] <- max(c((aov.tbl[2, 3] - aov.tbl[3, 3])/(n.part * n.within), 0))  ## Appraiser Reproducibility: (MSap - MSrepe)/(a*n)
    ev[reproducibility_tot,       calculated_value] <- ev[reproducibility_appraiser, calculated_value]  ## Reproducibility = Appr repro
    ev[variability_part_part,     calculated_value] <- max(c((aov.tbl[1, 3] - aov.tbl[3, 3])/(n.appraiser * n.within),0)) ## Part-to-part variation: (MSpart - MSrepe)/(b*n)
    ev[gage_tot_rr,               calculated_value] <- ev[repeatability, calculated_value] + ev[reproducibility_tot, calculated_value] ## Total GRR
    ev[variability_tot,           calculated_value] <- ev[gage_tot_rr, calculated_value] + ev[variability_part_part, calculated_value] ## Total Variation

  } else if (nrow(aov.tbl) == 5) {
    #2+ full model (with interaction)
    n.part      <- aov.tbl$Df[1]+1
    n.appraiser <- aov.tbl$Df[2]+1
    n.within    <- sum(aov.tbl$Df,1)/(n.part*n.appraiser)

    ev[repeatability,             calculated_value] <- aov.tbl[4, 3] # repeatability
    ev[reproducibility_appraiser, calculated_value] <- max(c((aov.tbl[2, 3] - aov.tbl[3, 3])/(n.part * n.within), 0))  # Appraiser Reproducibility
    ev[reproducibility_appr_part, calculated_value] <- max(c((aov.tbl[3, 3] - aov.tbl[4, 3])/n.within, 0))  # part:Appr Reproducibility
    ev[reproducibility_tot,       calculated_value] <- ev[reproducibility_appraiser, calculated_value] + ev[reproducibility_appr_part, calculated_value]  # total reproducibility
    ev[variability_part_part,     calculated_value] <- max(c((aov.tbl[1, 3] - aov.tbl[3, 3])/(n.appraiser * n.within),0))  # part to part
    ev[gage_tot_rr,               calculated_value] <- ev[repeatability, calculated_value] + ev[reproducibility_tot, calculated_value]  # Total GRR
    ev[variability_tot,           calculated_value] <- ev[gage_tot_rr, calculated_value] + ev[variability_part_part, calculated_value]  # Total Variation

  } else {
    stop("different postprocessed aov table than expected")
  }

  ev[, contribution_pct]    <- round(100 * (ev[, calculated_value]/ev[variability_tot, calculated_value]), 2)  
  ev[, standard_dev]        <- sqrt(ev[, calculated_value])  
  ev[, study_variation]     <- ev[, standard_dev] * n.sigma  
  ev[, study_variation_pct] <- round(100 * (ev[, standard_dev]/ev[variability_tot, standard_dev]), 2)
  ev[, tolerance_pct]       <- round(100 * (ev[, study_variation]/(stat.usl - stat.lsl)), 2)

  ev
}