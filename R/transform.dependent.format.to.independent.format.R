transform.dependent.format.to.independent.format <- function(
  data
) {

  data.frame(
    cell = as.vector(sapply(names(data), function(x) { rep(x, nrow(data)) }))
    ,measure = rmnames(unlist(c(data)))
  )

}
