msa.postprocess.aov.continuous.multi.appraiser.nointeraction.error <- function(
  model.aov
) {
  model.aov[[1]] <- rbind(
      model.aov[[1]] 
      ,c(
          colSums(model.aov[[1]][, 1:2])
          ,rep(NA, 3)
      )
  )
  
  rownames(model.aov[[1]])[3] <- "Repeatability"
  rownames(model.aov[[1]])[4] <- "Total"

  model.aov
}