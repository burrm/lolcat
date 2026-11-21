lolcat.core.coalesce <- function(...) {
  q <- list(...)
  unlist(lapply(q, FUN = function(l) {na.omit(l)}))[1]
}
