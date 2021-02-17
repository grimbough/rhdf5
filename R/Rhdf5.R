
h5listIdentifier <- function() {
  res <- .Call("_h5listIdentifier", PACKAGE='rhdf5')
  res$type = h5const2String("H5I_TYPE",res$type)
  res = as.data.frame(res)
  res
}

h5validObjects <- function(native = FALSE) {
  ids <- .Call("_h5validObjects", PACKAGE='rhdf5')
  res <- list()
  for (i in seq_len(length(ids))) {
    res[[i]] <- new("H5IdComponent", ID = ids[i], native = native)
  }
  res
}

h5listOpenObjects <- function(h5file) {
  h5checktype(h5file, "file")
  invisible(.Call("_h5listOpenObjects", h5file@ID, PACKAGE = "rhdf5"))
}

getDatatypeName <- function(type) {
  .Call("_getDatatypeName", type, PACKAGE='rhdf5')
}

getDatatypeClass <- function(type) {
  .Call("_getDatatypeClass", type, PACKAGE='rhdf5')
}

h5version <- function() {

  part1 <- paste0("This is Bioconductor rhdf5 ",
                  as.character(packageVersion("rhdf5")),
                  " linking to C-library HDF5 ",
                  paste(H5get_libversion(), collapse="."))
  
  part2 <- ifelse(requireNamespace('rhdf5filters'),
                  paste0(" and rhdf5filters ", 
                         as.character(packageVersion('rhdf5filters'))), 
                  "")
  message(part1, part2)
  invisible(NULL)
}
