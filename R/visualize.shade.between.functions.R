visualize.shade.between.functions <- function(
  f1.x = function(x) {rep(0, length(x))}
  ,f2.x = function(x) {sin(x)}
  ,from = pi/2
  ,to = 3*pi/2
  ,step = .001
  ,col = "lightblue"
  ,border = NULL
) {
  x <- seq(from = from, to = to, by = step)
  
  f1.points <- f1.x(x)
  f2.points <- f2.x(x)
  
  polygon(
    x = c(x, rev(x))
    ,y = c(f1.points, rev(f2.points))
    ,col= col
    ,border = border 
  )
}
