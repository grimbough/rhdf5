
H5Dcreate <- function( h5loc, name, dtype_id, h5space, internal1=NULL, internal2 =6 ) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  if (!is.integer(dtype_id)) {
    dtype_id<- h5checkConstants( "H5T", dtype_id)
  }
  stopifnot( is( h5space, "H5space" ) )
  chunk = internal1
  level = internal2
  if (!is.null(chunk)) { chunk = rev(as.integer(chunk)) }
  if (!is.null(level)) { level = as.integer(level) }
  did <- .Call("_H5Dcreate", h5loc@ID, name, dtype_id, h5space@ID, chunk, level, PACKAGE='rhdf5')
  if (did > 0) {
    h5dataset = new("H5dataset", ID = did)
  } else {
    message("HDF5: unable to create dataset")
    h5dataset = FALSE
  }
  invisible(h5dataset)
}

H5Dopen <- function( h5loc, name ) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
  if (length(name)!=1 || !is.character(name)) stop("'filename' must be a character string of length 1")
  did <- .Call("_H5Dopen", h5loc@ID, name, PACKAGE='rhdf5')
  if (did > 0) {
    h5dataset = new("H5dataset", ID = did)
  } else {
    message("HDF5: unable to open dataset")
    h5dataset = FALSE
  }
  invisible(h5dataset)
}

H5Dclose <- function( h5dataset ) {
  stopifnot( is( h5dataset, "H5dataset" ) )
  invisible(.Call("_H5Dclose", h5dataset@ID, PACKAGE='rhdf5'))
}

H5Dget_space <- function( h5dataset ) {
  stopifnot( is( h5dataset, "H5dataset" ) )
  sid <- .Call("_H5Dget_space", h5dataset@ID, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5space", ID = sid)
  } else {
    message("HDF5: unable to create simple data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Dread <- function( h5dataset, h5spaceFile=NULL, h5spaceMem=NULL, buf = NULL, compoundAsDataFrame = TRUE) {
  stopifnot( is( h5dataset, "H5dataset" ) )
  stopifnot( is.null(h5spaceFile) | is( h5spaceFile, "H5space" ) )
  stopifnot( is.null(h5spaceMem) | is( h5spaceMem, "H5space" ) )
  if (is.null(h5spaceMem)) { sidMem <- NULL } else { sidMem <- h5spaceMem@ID }
  if (is.null(h5spaceFile)) { sidFile <- NULL } else { sidFile <- h5spaceFile@ID }
  .Call("_H5Dread", h5dataset@ID, sidFile, sidMem, buf, compoundAsDataFrame, PACKAGE='rhdf5')
}

H5Dwrite <- function( h5dataset, buf, h5spaceMem=NULL, h5spaceFile=NULL ) {
  stopifnot( is( h5dataset, "H5dataset" ) )
  stopifnot( is.null(h5spaceMem) | is( h5spaceMem, "H5space" ) )
  stopifnot( is.null(h5spaceFile) | is( h5spaceFile, "H5space" ) )
  if (is.null(h5spaceMem)) { sidMem <- NULL } else { sidMem <- h5spaceMem@ID }
  if (is.null(h5spaceFile)) { sidFile <- NULL } else { sidFile <- h5spaceFile@ID }
  invisible(.Call("_H5Dwrite", h5dataset@ID, buf, sidFile, sidMem, PACKAGE='rhdf5'))
}

