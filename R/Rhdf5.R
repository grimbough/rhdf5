
h5listIdentifier <- function() {
  res <- .Call("_h5listIdentifier", PACKAGE='rhdf5')
  res$type = h5const2Factor("H5I_TYPE",res$type)
  res = as.data.frame(res)
  res
}

h5validObjects <- function() {
  ids <- .Call("_h5validObjects", PACKAGE='rhdf5')
  res <- list()
  for (i in seq_len(length(ids))) {
    res[[i]] <- new("H5IdComponent", ID = ids[i])
  }
  res
}

getDatatypeName <- function(type) {
  .Call("_getDatatypeName", type, PACKAGE='rhdf5')
}

getDatatypeClass <- function(type) {
  .Call("_getDatatypeClass", type, PACKAGE='rhdf5')
}

h5version <- function() {
  cat("This is Bioconductor rhdf5",as.character(packageVersion("rhdf5")),
      "linking to C-library HDF5",
      paste(H5get_libversion(),collapse="."),"\n")
  invisible(NULL)
}
