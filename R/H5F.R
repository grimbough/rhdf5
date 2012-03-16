
H5Fcreate <- function( name, flags = h5default("H5F_ACC") ) {
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  flags <- h5checkConstants( "H5F_ACC", flags )
  fid <- .Call("_H5Fcreate", name, flags, PACKAGE='rhdf5')
  if (fid > 0) {
    h5file = new("H5IdComponent", ID = fid)
  } else {
    message("HDF5: unable to create file")
    h5file = FALSE
  }
  invisible(h5file)
}

H5Fopen <- function( name, flags = h5default("H5F_ACC_RD") ) {
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  flags <- h5checkConstants( "H5F_ACC_RD", flags )
  fid <- .Call("_H5Fopen", name, flags, PACKAGE='rhdf5')
  if (fid > 0) {
    h5file = new("H5IdComponent", ID = fid)
  } else {
    message("HDF5: unable to open file")
    h5file = FALSE
  }
  invisible(h5file)
}

## H5Freopen <- function( h5file ) {
##   ## h5checktype(h5file, "file")
##   fid <- .Call("_H5Freopen", h5file@ID, PACKAGE='rhdf5')
##   if (fid > 0) {
##     h5file = new("H5file", ID = fid)
##   } else {
##     message("HDF5: unable to open file")
##     h5file = FALSE
##   }
##   invisible(h5file)
## }

H5Fclose <- function( h5file ) {
  h5checktype(h5file, "file")
  invisible(.Call("_H5Fclose", h5file@ID, PACKAGE='rhdf5'))
}

H5Fflush <- function( h5file, scope = h5default("H5F_SCOPE") ) {
  h5checktype(h5file, "file")
  scope <- h5checkConstants( "H5F_SCOPE", scope )
  invisible(.Call("_H5Fflush", h5file@ID, scope, PACKAGE='rhdf5'))
}




