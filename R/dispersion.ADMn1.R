dispersion.ADMn1 <- function(x) {
  x.adm   <- dispersion.ADM(x)
  min.adm <- min(x.adm)
  x.adm[which(x.adm == min.adm)[1]]<-NA
  x.adm
}