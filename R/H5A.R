#' Create an attribute for an HDF5 object
#' 
#' Creates an attribute, `name`, which is attached to the object specified 
#' by the identifier `h5obj`. The attribute name must be unique for the object.
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing a H5 object 
#' identifier (file, group, or dataset). See [H5Fcreate()], [H5Fopen()], 
#' [H5Gcreate()], [H5Gopen()], [H5Dcreate()], or [H5Dopen()] to create an object of this kind.
#' @param name The name of the attribute (character).
#' @param dtype_id A character name of a datatype. See \code{h5const("H5T")} for 
#' possible datatypes. Can also be an integer representing an HDF5 datatype. 
#' Only simple datatypes are allowed for atttributes.
#' @param h5space An object of class [H5IdComponent-class] representing a H5 
#' dataspace. See [H5Dget_space()], [H5Screate_simple()], [H5Screate()] to create an object 
#' of this kind.
#' 
#' @return An object of class [H5IdComponent-class] representing a H5 attribute identifier.
#' 
#' @export
H5Acreate <- function( h5obj, name, dtype_id, h5space ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  ##if (!is.integer(dtype_id)) {
  ## dont check if we have an H5T identifier already    
  if (!grepl(pattern = "^[[:digit:]]+$", dtype_id)) {
    dtype_id<- h5checkConstants( "H5T", dtype_id)
  }
  h5checktype(h5space, "dataspace")
  aid <- .Call("_H5Acreate", h5obj@ID, name, dtype_id, h5space@ID, PACKAGE='rhdf5')
  if (aid > 0) {
    h5attribute = new("H5IdComponent", ID = aid, native = h5obj@native)
  } else {
    message("HDF5: unable to create attribute")
    h5attribute = FALSE
  }
  invisible(h5attribute)
}

#' Open an attribute for an HDF5 object
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing a H5 object 
#' identifier (file, group, or dataset). See [H5Fcreate()], [H5Fopen()], 
#' [H5Gcreate()], [H5Gopen()], [H5Dcreate()], or [H5Dopen()] to create an object of this kind.
#' @param name The name of the attribute (character).
#' @param objname The name of the object the attribute belongs to.
#' @param n Opens attribute number `n` in the given order and index. Indexing is C-style, base-0, 
#' so the first attribute is opened with `n=0`.
#' @param index_type See \code{h5const("H5_INDEX")} for possible arguments.
#' @param order See \code{h5const("H5_ITER")} for possible arguments.
#' 
#' @return An object of class [H5IdComponent-class] representing a H5 attribute identifier.
#' 
#' @name H5Aopen
NULL

#' @rdname H5Aopen
#' @export
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

#' @rdname H5Aopen
#' @export
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

#' @rdname H5Aopen
#' @export
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

#' Check whether an specific attribute exists for an HDF5 object
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing a H5 object 
#' identifier (file, group, or dataset). See [H5Fcreate()], [H5Fopen()], 
#' [H5Gcreate()], [H5Gopen()], [H5Dcreate()], or [H5Dopen()] to create an object of this kind.
#' @param name The name of the attribute (character).
#' 
#' @export
H5Aexists <- function( h5obj, name ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  res <- .Call("_H5Aexists", h5obj@ID, name, PACKAGE='rhdf5')
  res <- ifelse(res > 0, TRUE, FALSE)
  res
}


#' Close an HDF5 attribute
#' 
#' @param h5attribute An object of class [H5IdComponent-class] representing a 
#' the attribute to be closed.  Normally created by [H5Aopen()] or similar.
#' 
#' @seealso [H5Aopen()] 
#' @export
H5Aclose <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  invisible(.Call("_H5Aclose", h5attribute@ID, PACKAGE='rhdf5'))
}

#' Delete an specified attribute of an HDF5 object
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing a H5 object 
#' identifier (file, group, or dataset). See [H5Fcreate()], [H5Fopen()], 
#' [H5Gcreate()], [H5Gopen()], [H5Dcreate()], or [H5Dopen()] to create an object of this kind.
#' @param name The name of the attribute (character).
#' 
#' @export
H5Adelete <- function( h5obj, name ) {
  h5checktype(h5obj, "object")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  res <- .Call("_H5Adelete", h5obj@ID, name, PACKAGE='rhdf5')
  res
}

#' Get the name of an HDF5 attribute object
#' 
#' Retrieves the name of the attribute specified by an HDF5 attribute object.
#' 
#' @param h5attribute An object of class [H5IdComponent-class] representing an
#' attribute.  Normally created by [H5Aopen()] or similar.
#' 
#' @return A character vector of length 1 containing the name of the attribute.
#' 
#' @export
H5Aget_name <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  name <- .Call("_H5Aget_name", h5attribute@ID, PACKAGE='rhdf5')
  name
}

#' Get a copy of the attribute dataspace
#' 
#' @param h5attribute An object of class [H5IdComponent-class] representing an
#' attribute.  Normally created by [H5Aopen()] or similar.
#' 
#' @return Returns an object of class [H5IdComponent-class] representing a H5 
#' dataspace identifier
#' 
#' @export
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

#' Get a copy of the attribute datatype
#' 
#' @param h5attribute An object of class [H5IdComponent-class] representing an
#' attribute.  Normally created by [H5Aopen()] or similar.
#' 
#' @export
H5Aget_type <- function( h5attribute ) {
  h5checktype(h5attribute, "attribute")
  tid <- .Call("_H5Aget_type", h5attribute@ID, PACKAGE='rhdf5')
  invisible(tid)
}

#' Read data from an HDF5 attribute
#'
#' @details Internally, R does not support 64-bit integers. All integers in R are 32-bit
#' integers. By setting bit64conversion='int', a coercing to 32-bit integers is
#' enforced, with the risk of data loss, but with the insurance that numbers 
#' are represented as integers. bit64conversion='double' coerces the 64-bit 
#' integers to floating point numbers. doubles can represent integers with up 
#' to 54-bits, but they are not represented as integer values anymore. For 
#' larger numbers there is again a data loss. bit64conversion='bit64' is 
#' recommended way of coercing. It represents the 64-bit integers as objects 
#' of class 'integer64' as defined in the package 'bit64'. Make sure that you 
#' have installed 'bit64'. The datatype 'integer64' is not part of base R, but 
#' defined in an external package. This can produce unexpected behaviour when 
#' working with the data.
#' 
#' @param h5attribute An object of class [H5IdComponent-class] representing an
#' attribute.  Normally created by [H5Aopen()] or similar.
#' @param buf Optional buffer to store retrieved values. The buffer size has to
#' fit the size of the memory space \code{h5spaceMem}. No extra memory will be 
#' allocated for the data. Default is `NULL` which means the function will 
#' return the attribute data.
#' @param bit64conversion Defines how 64-bit integers are converted. (See
#' the details section for more information on these options.)
#' 
#' @return If `buf=NULL` returns the contents of the attribute.  Otherwise 
#' return 0 if attribute is read successfully.
#' 
#' @export
H5Aread <- function(h5attribute, buf = NULL, bit64conversion) {
  h5checktype(h5attribute, "attribute")

  if (missing(bit64conversion)) {
    bit64conv = 0L
  } else {
    bit64conv = switch(bit64conversion, int = 0L, double = 1L, bit64 = 2L, default = 0L)
  }
  if (bit64conv == 2L && !requireNamespace("bit64",quietly=TRUE)) {
      stop("install package 'bit64' before using bit64conversion='bit64'")
  }

  invisible(.Call("_H5Aread", h5attribute@ID, buf, bit64conv, PACKAGE='rhdf5'))
}

#' Write data to an HDF5 attribute
#' 
#' @param h5attribute An object of class [H5IdComponent-class] representing an
#' attribute.  Normally created by [H5Aopen()] or similar.
#' @param buf The data to be written.
#' 
#' @export
H5Awrite <- function(h5attribute, buf) {
  h5checktype(h5attribute, "attribute")
  invisible(.Call("_H5Awrite", h5attribute@ID, buf, PACKAGE='rhdf5'))
}

