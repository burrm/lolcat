compute.group.dispersion.ADA <- function(
  fx          #Formula defining groups
  ,data = NULL #data frame
) {
  compute.group.variable(fx = fx,
                         data = data,
                         FUN = dispersion.ADA)
}