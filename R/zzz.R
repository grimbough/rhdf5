.First.lib <- function(lib, pkg, where) {
  library.dynam("rhdf5",pkg,lib)
  .Call("HDF_init")
  if (missing(where)) {
      where <- match(paste("package:", pkg, sep = ""),
                     search())
      if (is.na(where)) {
          warning(paste("Not a package name: ", pkg))
          return()
      }
      where <- as.environment(where)
  }

  .initHDF5(where)
}

