compute.group.dispersion.ADM <- function(
  fx          #Formula defining groups
  ,data = NULL #data frame
) {
  compute.group.variable(fx = fx,
                         data = data,
                         FUN = dispersion.ADM)
}