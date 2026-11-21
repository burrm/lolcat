msa.postprocess.aov.continuous.one.appraiser <- function(model.aov) { 
  rownames(model.aov[[1]])[2] <- "Repeatability"

  model.aov[[1]] <- rbind(
    model.aov[[1]], 
    c(
        colSums(model.aov[[1]][, 1:2])
        ,rep(NA, 3)
    )
  )

  model.aov
}