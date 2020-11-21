utility.extract.named.value <- function(
  obj = c(a= 1, b = 2, c= 3), #named atomic
  name = "b"                  #name
) {
  idx <- which(names(obj) == name)
  rmnames(obj[idx])
}