H5PLprepend <- function(path) {
  res <- .Call("_H5PLprepend", path, PACKAGE = 'rhdf5')
  invisible(res)
}


H5PLsize <- function() {
  size <- .Call("_H5PLsize", PACKAGE = 'rhdf5')
  return(size)
}

H5PLget <- function(index) {
  path <- .Call("_H5PLget", as.integer(index - 1), PACKAGE = "rhdf5")
  return(path)
}

h5getPluginPaths <- function() {
  n_paths <- H5PLsize()
  paths <- vapply(seq_len(n_paths), FUN = H5PLget, FUN.VALUE = character(1))
  return(paths)
}
