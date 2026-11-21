msa.postprocess.aov.continuous.multi.appraiser.interaction.error <- function(
  model.aov
) {
  # Assumes both part and appraiser are random factors.

  model.aov[[1]][1:2, 4] <- model.aov[[1]][1:2, 3]/model.aov[[1]][3, 3]
  
  model.aov[[1]][1:2, 5] <- pf(
      model.aov[[1]][1:2, 4] 
      ,model.aov[[1]][1:2, 1] 
      ,model.aov[[1]][3, 1]
      ,lower.tail = FALSE
  )

  model.aov[[1]] <- rbind(
      model.aov[[1]] 
      ,c(
          colSums(model.aov[[1]][, 1:2])
          ,rep(NA, 3)
      )
  )
  
  rownames(model.aov[[1]])[4] <- "Repeatability"
  rownames(model.aov[[1]])[5] <- "Total"

  model.aov
}