
.onLoad <- function(libname, pkgname) {
  h5constants <<- H5loadConstants()
  h5errorHandling()
}
