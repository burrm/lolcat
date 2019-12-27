.onAttach <- function(libname, pkgname) {
    this.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                      fields="Version")
    packageStartupMessage(paste(pkgname, this.version))
    packageStartupMessage("Open Source version. Commercial support is available for this package and recommended for organizations. To inquire, please contact Mike Burr at Michael.Burr@BurrAndSons.com for more information.")
}