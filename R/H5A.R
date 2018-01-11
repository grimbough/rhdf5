
H5Acreate <- function( h5obj, name, dtype_id, h5space ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  if (!is.integer(dtype_id)) {
    dtype_id<- h5checkConstants( "H5T", dtype_id)
  }
  h5checktype(h5space, "dataspace")
  aid <- .Call("_H5Acreate", h5obj@ID, name, dtype_id, h5space@ID, PACKAGE='rhdf5')
  if (aid > 0) {
    h5attribute = new("H5IdComponent", ID = aid, native = .natve(h5obj))
  } else {
    message("HDF5: unable to create attribute")
    h5attribute = FALSE
  }
  invisible(h5attribute)
}

H5Aopen <- function( h5obj, name ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  aid <- .Call("_H5Aopen", h5obj@ID, name, PACKAGE='rhdf5')
  if (aid > 0) {
    h5attribute = new("H5IdComponent", ID = aid, native = h5obj@native)
  } else {
    message("HDF5: unable to open attribute")
    h5attribute = FALSE
  }
  invisible(h5attribute)
}

H5Aopen_by_name <- function( h5obj, objname = ".", name ) {
  h5checktype(h5obj, "object")
  if (length(objname)!=1 || !is.character(objname)) stop("'objname' must be a character string of length 1")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  aid <- .Call("_H5Aopen_by_name", h5obj@ID, objname, name, PACKAGE='rhdf5')
  if (aid > 0) {
    h5attribute = new("H5IdComponent", ID = aid, native = h5obj@native)
  } else {
    message("HDF5: unable to open attribute")
    h5attribute = FALSE
  }
  invisible(h5attribute)
}

H5Aopen_by_idx <- function( h5obj, n, objname=".", index_type = h5default("H5_INDEX"), order = h5default("H5_ITER")) {
  h5checktype(h5obj, "object")
  if ((length(objname)!=1) || !is.character(objname)) stop("'objname' must be a character string of length 1")
  index_type <- h5checkConstants( "H5_INDEX", index_type )
  order <- h5checkConstants( "H5_ITER", order )
  if ((length(n) !=1) || !is.numeric(n)) stop("'n' must be an integer of length 1")
  n = as.integer(n)
  aid <- .Call("_H5Aopen_by_idx", h5obj@ID, objname, index_type, order, n, PACKAGE='rhdf5')
  if (aid > 0) {
    h5attribute = new("H5IdComponent", ID = aid, native = h5obj@native)
  } else {
    message("HDF5: unable to open attribute")
    h5attribute = FALSE
  }
  invisible(h5attribute)
}

H5Aexists <- function( h5obj, name ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  res <- .Call("_H5Aexists", h5obj@ID, name, PACKAGE='rhdf5')
  res <- ifelse(res > 0, TRUE, FALSE)
  res
}

H5Aclose <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  invisible(.Call("_H5Aclose", h5attribute@ID, PACKAGE='rhdf5'))
}

H5Adelete <- function( h5obj, name ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  res <- .Call("_H5Adelete", h5obj@ID, name, PACKAGE='rhdf5')
  res
}

H5Aget_name <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  name <- .Call("_H5Aget_name", h5attribute@ID, PACKAGE='rhdf5')
  name
}

H5Aget_space <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  sid <- .Call("_H5Aget_space", h5attribute@ID, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5IdComponent", ID = sid, native = h5attribute@native)
  } else {
    message("HDF5: unable to create simple data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Aget_type <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  tid <- .Call("_H5Aget_type", h5attribute@ID, PACKAGE='rhdf5')
  invisible(tid)
}

H5Aread <- function(h5attribute, buf = NULL) {
  h5checktype(h5attribute, "attribute")
  invisible(.Call("_H5Aread", h5attribute@ID, buf, PACKAGE='rhdf5'))
}

H5Awrite <- function(h5attribute, buf) {
  h5checktype(h5attribute, "attribute")
  invisible(.Call("_H5Awrite", h5attribute@ID, buf, PACKAGE='rhdf5'))
}

