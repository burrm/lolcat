#' Calculate Subgroup Dispersion - Median Absolute Deviation
#' 
#' Calculate subgroup dispersion using absolute deviation from subgroup median.
#'
#' @param fx Formula - defines formula for dependent variable and subgroups ( dv ~ v1 + v2 + ...)
#' @param data Data Frame - data frame with columns corresponding to variables in fx. 
#'
#' @return A vector containing absolute deviations from subgroup medians.

compute.group.dispersion.ADM <- function(
  fx          #Formula defining groups
  ,data = NULL #data frame
) {
  compute.group.variable(fx = fx,
                         data = data,
                         FUN = dispersion.ADM)
}