msa.postprocess.ev.number.distinct.categories.simple <- function(
  study.variation.part.part,
  study.variation.total
) {
  max(c(1, floor(sqrt(2)*(study.variation.part.part/study.variation.total))))  
}