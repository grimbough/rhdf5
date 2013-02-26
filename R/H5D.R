
H5Dcreate <- function( h5loc, name, dtype_id, h5space, internal1=NULL, internal2 =6 ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  if (!is.integer(dtype_id)) {
    dtype_id<- h5checkConstants( "H5T", dtype_id)
  }
  h5checktype(h5space, "dataspace")
  chunk = internal1
  level = internal2
  if (!is.null(chunk)) { chunk = rev(as.integer(chunk)) }
  if (!is.null(level)) { level = as.integer(level) }
  did <- .Call("_H5Dcreate", h5loc@ID, name, dtype_id, h5space@ID, chunk, level, PACKAGE='rhdf5')
  if (did > 0) {
    h5dataset = new("H5IdComponent", ID = did)
  } else {
    message("HDF5: unable to create dataset")
    h5dataset = FALSE
  }
  invisible(h5dataset)
}

H5Dopen <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'filename' must be a character string of length 1")
  did <- .Call("_H5Dopen", h5loc@ID, name, PACKAGE='rhdf5')
  if (did > 0) {
    h5dataset = new("H5IdComponent", ID = did)
  } else {
    message("HDF5: unable to open dataset")
    h5dataset = FALSE
  }
  invisible(h5dataset)
}

H5Dclose <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  invisible(.Call("_H5Dclose", h5dataset@ID, PACKAGE='rhdf5'))
}

H5Dget_type <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  tid <- .Call("_H5Dget_type", h5dataset@ID, PACKAGE='rhdf5')
  invisible(tid)
}

H5Dget_space <- function( h5dataset ) {
  h5checktype(h5dataset, "dataset")
  sid <- .Call("_H5Dget_space", h5dataset@ID, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5IdComponent", ID = sid)
  } else {
    message("HDF5: unable to create simple data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Dread <- function( h5dataset, h5spaceFile=NULL, h5spaceMem=NULL, buf = NULL, compoundAsDataFrame = TRUE,
                     bit64conversion ) {
  h5checktype(h5dataset, "dataset")
  h5checktypeOrNULL(h5spaceFile, "dataspace")
  h5checktypeOrNULL(h5spaceMem, "dataspace")
  if (is.null(h5spaceMem)) { sidMem <- NULL } else { sidMem <- h5spaceMem@ID }
  if (is.null(h5spaceFile)) { sidFile <- NULL } else { sidFile <- h5spaceFile@ID }
  if (missing(bit64conversion)) {
    bit64conv = 0L
  } else {
    bit64conv = switch(bit64conversion, int = 1L,double = 2L,bit64 = 3L,default=0L)
  }
  if (bit64conv == 3L) {
    if (!require(bit64,quietly=TRUE)) {
      stop("You have to install the package 'bit64' before you can use the option bit64conversion='bit64'")
    }
  }
  .Call("_H5Dread", h5dataset@ID, sidFile, sidMem, buf, compoundAsDataFrame, bit64conv, PACKAGE='rhdf5')
}

H5Dwrite <- function( h5dataset, buf, h5spaceMem=NULL, h5spaceFile=NULL ) {
  h5checktype(h5dataset, "dataset")
  h5checktypeOrNULL(h5spaceFile, "dataspace")
  h5checktypeOrNULL(h5spaceMem, "dataspace")
  if (is.null(h5spaceMem)) { sidMem <- NULL } else { sidMem <- h5spaceMem@ID }
  if (is.null(h5spaceFile)) { sidFile <- NULL } else { sidFile <- h5spaceFile@ID }
  invisible(.Call("_H5Dwrite", h5dataset@ID, buf, sidFile, sidMem, PACKAGE='rhdf5'))
}

