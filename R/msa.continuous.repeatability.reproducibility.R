#' Perform Gage Repeatability and Reproducibility Study
#' 
#' Uses either 1 way (single appraiser) analysis of variance or 2-way (multi appraiser) random effects analysis of variance. 
#' Calculates components of variance and study variation.
#'
#' @param measurement A vector of values used as measurements
#' @param part A vector/factor of values used as part identifiers
#' @param appraiser A vector/factor of values used as appraiser identifiers
#' @param conf.level.interaction Confidence level to drop interaction and use combined error term for ANOVA
#' @param stat.lsl Lower specification limit (if applicable)
#' @param stat.usl Upper specification limit (if applicable)
#'
#' @return A list including all results. Recommended pieces to use are summary.aov.reduced, 
#'         ev.reduced, summary.reduced, ev.reduced.number.distinct.categories, and ev.reduced.discrimination.ratio  


#Loosely based on SixSigma package
msa.continuous.repeatability.reproducibility <- function(
  measurement
  ,part                   = rep(1, length(measurement))
  ,appraiser              = rep(1, length(measurement))
  ,conf.level.interaction = .95
  ,stat.lsl               = NA
  ,stat.usl               = NA
) {
  ret <- list()

  if (is.factor(measurement)) {
    warning("measurement should be continuous variable, but is a factor.")
  }

  if (!is.factor(part)) {
    part <- factor(part)
  }

  if (!is.factor(appraiser)) {
    appraiser <- factor(appraiser)
  }  
  
  data.in <- data.frame(measurement, part, appraiser)
  
  n.part      <- nlevels(part)
  n.appraiser <- nlevels(appraiser)
  n.within    <- length(measurement)/(n.part*n.appraiser)

  ret$input <- list(
    measurement             = measurement 
    ,part                   = part 
    ,appraiser              = appraiser
    ,conf.level.interaction = conf.level.interaction
    ,stat.lsl               = stat.lsl
    ,stat.usl               = stat.usl
  )

  ret$n.part      <- n.part
  ret$n.appraiser <- n.appraiser
  ret$n.within    <- n.within

  if (n.appraiser == 1) {
    model.aov <- aov(
      measurement ~ part,
      data = data.in
    )
    
    ret$raw.aov.full <- model.aov
    model.aov <- summary(model.aov)

    model.aov <- msa.postprocess.aov.continuous.one.appraiser(model.aov)
    ret$summary.aov.full <- model.aov

    model.ev <- msa.postprocess.aov.continuous.explained.variability(
      model.aov = model.aov
      ,stat.lsl = stat.lsl
      ,stat.usl = stat.usl
      ,n.sigma  = 6
    )

    ret$ev.full <- model.ev
    ret$ev.full.number.distinct.categories <- msa.postprocess.ev.number.distinct.categories(model.ev)
    ret$ev.full.discrimination.ratio <- msa.postprocess.ev.number.discrimination.ratio(model.ev)
    
    ret$summary.full <- msa.postprocess.continuous.summarize(
      model.aov = model.aov
      ,model.ev = model.ev
    )

    ret$raw.aov.reduced <- ret$raw.aov.full
    ret$summary.aov.reduced <- ret$summary.aov.full
    ret$ev.reduced <- ret$ev.full
    ret$ev.reduced.number.distinct.categories <- ret$ev.full.number.distinct.categories
    ret$ev.reduced.discrimination.ratio <- ret$ev.full.discrimination.ratio
    ret$summary.reduced <- ret$summary.full
    
    ret$summary.reduced <- ret$summary.reduced[c(-3:-5),]
    ret$ev.reduced <- ret$ev.reduced[c(-3:-5),]

  } else {

    #print("aov")

    model.aov <- aov(
      measurement ~ part * appraiser #+ Error(part / appraiser)
      ,data = data.in
    )

    #print("summary aov")

    ret$raw.aov.full <- model.aov
    model.aov <- summary(model.aov)

    #print("postprocess aov")

    model.aov <- msa.postprocess.aov.continuous.multi.appraiser.interaction.error(model.aov)
    ret$summary.aov.full <- model.aov

    #print("explained variability")

    model.ev <- msa.postprocess.aov.continuous.explained.variability(
      model.aov = model.aov
      ,stat.lsl = stat.lsl
      ,stat.usl = stat.usl
      ,n.sigma  = 6
    )

    ret$ev.full <- model.ev
    ret$ev.full.number.distinct.categories <- msa.postprocess.ev.number.distinct.categories(model.ev)
    ret$ev.full.discrimination.ratio <- msa.postprocess.ev.number.discrimination.ratio(model.ev)
    
    #print("summarize")

    ret$summary.full <- msa.postprocess.continuous.summarize(
      model.aov = model.aov
      ,model.ev = model.ev
    )

    #TODO: Final summary object from aov and explained variability

    if (model.aov[[1]][3,5] > (1-conf.level.interaction)) {
      model.aov.reduced <- aov(
         measurement ~ part + appraiser,
         data = data.in
      )

      ret$raw.aov.reduced <- model.aov.reduced
      model.aov.reduced <- summary(model.aov.reduced)

      model.aov.reduced <- msa.postprocess.aov.continuous.multi.appraiser.nointeraction.error(model.aov.reduced)
      ret$summary.aov.reduced <- model.aov.reduced

      model.ev <- msa.postprocess.aov.continuous.explained.variability(
        model.aov = model.aov.reduced
        ,stat.lsl = stat.lsl
        ,stat.usl = stat.usl
        ,n.sigma  = 6
      )

      ret$ev.reduced <- model.ev
      ret$ev.reduced.number.distinct.categories <- msa.postprocess.ev.number.distinct.categories(model.ev)
      ret$ev.reduced.discrimination.ratio <- msa.postprocess.ev.number.discrimination.ratio(model.ev)
    
      ret$summary.reduced <- msa.postprocess.continuous.summarize(
        model.aov = model.aov.reduced
        ,model.ev = model.ev
      )


      ret$summary.reduced <- ret$summary.reduced[c(-5),]
      ret$ev.reduced <- ret$ev.reduced[c(-5),]
      
    } else {

      ret$raw.aov.reduced <- ret$raw.aov.full
      ret$summary.aov.reduced <- ret$summary.aov.full
      ret$ev.reduced <- ret$ev.full
      ret$ev.reduced.number.distinct.categories <- ret$ev.full.number.distinct.categories
      ret$ev.reduced.discrimination.ratio <- ret$ev.full.discrimination.ratio
      ret$summary.reduced <- ret$summary.full

    }

  }

  




  class(ret) <- "lolcat.msa.continuous.repeatability.reproducibility"
  
  ret
}