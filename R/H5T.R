#' Copy an existing datatype
#' 
#' @param dtype_id Datatype to copy.  Can either be a character
#' specifying a predefined HDF5 datatype (see `h5const("H5T")` for valid 
#' options) or the ID of an already created datatype.
#' 
#' @export
H5Tcopy <- function( dtype_id = h5default(type="H5T")) {
  if (is.numeric(dtype_id)) {
    dtype_id <- as.integer(dtype_id)
  } else {
    dtype_id <- h5checkConstants( "H5T", dtype_id )
  }
  invisible(.Call("_H5Tcopy", dtype_id, PACKAGE='rhdf5'))
}

#' Retrieve or set the type of padding used by string datatype
#' 
#' @param dtype_id ID of HDF5 datatype to query or modify.
#' @param size The new datatype size in bytes.
#' 
#' @name H5T_size
NULL

#' @rdname H5T_size
#' @export
H5Tset_size <- function( dtype_id = h5default(type="H5T"), size) {
  # string constant type_id do not make sense, because they are not allowed to be changed
  if (!grepl(pattern = "^[[:digit:]]+$", dtype_id))
      stop("Argument 'dtype_id' should be created by a call to H5Tcopy()")
  size <- as.integer(size)
  invisible(.Call("_H5Tset_size", dtype_id, size, PACKAGE='rhdf5'))
}

#' @rdname H5T_size
#' @export
H5Tget_size <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  .Call("_H5Tget_size", dtype_id, PACKAGE='rhdf5')
}

#' Retrieve or set the type of padding used by string datatype
#' 
#' @param dtype_id ID of HDF5 datatype to query or modify.
#' @param strpad Character vector of length 1 specifying the type of padding
#' to use.  Valid options are `NULLTERM`, `NULLPAD` and `SPACEPAD`.
#' 
#' @name H5T_strpad
NULL

#' @rdname H5T_strpad
#' @export 
H5Tset_strpad <- function( dtype_id, strpad = "NULLPAD") {
  
  strpad_int <- switch(strpad, 
                       NULLTERM = 0L, 
                       NULLPAD = 1L, 
                       SPACEPAD = 2L, 
                       stop("Invalid value to 'strpad' argument.\n",
                            "Valid options are: 'NULLTERM', 'NULLPAD', 'SPACEPAD'"))
  
  .Call("_H5Tset_strpad", dtype_id, strpad_int, PACKAGE = "rhdf5")
}
  
#' @rdname H5T_strpad
#' @export 
H5Tget_strpad <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  .Call("_H5Tget_strpad", dtype_id, PACKAGE = "rhdf5")
}

#' Retrieve or set the character set to be used in a string datatype.
#' 
#' @param dtype_id ID of HDF5 datatype to query or modify.
#' @param cset Encoding to use for string types. Valid options are 'ASCII' and 
#' 'UTF-8'.
#' 
#' @name H5T_cset
NULL

#' @rdname H5T_cset
#' @export
H5Tset_cset <- function( dtype_id, cset = "ASCII") {

    cset_int <- switch(cset, 
                     "ASCII" = 0L, 
                     "UTF8" = 1L,
                     "UTF-8" = 1L, 
                     stop("Invalid value to 'cset' argument.\n",
                          "Valid options are: 'ASCII', 'UTF-8'")) 
  
  .Call("_H5Tset_cset", dtype_id, cset_int, PACKAGE = "rhdf5")
}
  
#' @rdname H5T_cset
#' @export
H5Tget_cset <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  .Call("_H5Tget_cset", dtype_id, PACKAGE = "rhdf5")
}

#' Determine whether a datatype is a variable length string
#' 
#' @param dtype_id ID of HDF5 datatype to query.
#' 
#' @export
H5Tis_variable_str <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  res <- .Call("_H5Tis_variable_str", dtype_id, PACKAGE = "rhdf5")
  if(res < 0) {
    stop("Unable to determine whether datatype is a variable length string")
  } 
  
  return(as.logical(res))
}

#' Retrieve or set the precision of an HDF5 datatype
#'
#' @param dtype_id ID of HDF5 datatype to set precision of.
#' @param precision The number of bytes of precision for the datatype.
#' 
#' @returns 
#' * `H5Tget_precision()` returns an integer giving the number of 
#' significant bits used by the given datatype.
#' * `H5Tset_precision()` is call for its side-effect of modifying the 
#' precision of a datatype.  It will invisibly return `TRUE` if this is
#' successful and will stop with an error if the operation fails.
#'
#' @name H5T_precision
NULL

#' @rdname H5T_precision
#' @export
H5Tset_precision <- function( dtype_id, precision ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  precision <- as.integer(precision)
  if(precision < 1) {
    stop("'precision' argument must be greater than 0.")
  } 
  
  res <- .Call("_H5Tset_precision", dtype_id, as.integer(precision), PACKAGE = "rhdf5")
  if(res < 0) {
    stop("Unable to set data type precision")
  } 
  
  return(invisible(TRUE))
}

#' @rdname H5T_precision
#' @export
H5Tget_precision <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  precision <- .Call("_H5Tget_precision", dtype_id, PACKAGE = "rhdf5")
  
  return(precision)
  
}

#' Create or modify an HDF5 enum datatype
#'
#' @param dtype_id ID of HDF5 datatype to work with.  For `H5Tenum_create`, this
#'   is the identifier of the basedatype, and must be an integer e.g.
#'   `H5T_NATIVE_INT`. For `H5Tenum_insert` this will be a datatype identifier
#'   created by `H5Tenum_create`.
#' @param name The name of a the new enum member.  This is analogous to a
#'   "level" in an R factor.
#' @param value The value of the new member.  Must be compatible with the base
#'   datatype defined by `dtype_id`.
#'
#' @returns * `H5Tinsert_enum()` returns an character representing the H5 identifier
#' of the new datatype. * `H5Tset_precision()` is called
#'   for its side-effect of modifying the existing datatype.  It will
#'   invisibly return `TRUE` if this is successful `FALSE` if not.
#'   
#' @examples 
#' tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR")
#' H5Tenum_insert(tid, name = "TRUE", value = 1L)
#' H5Tenum_insert(tid, name = "FALSE", value = 0L)
#'
#' @name H5T_enum
NULL

#' @rdname H5T_enum
#' @export
H5Tenum_create <- function(dtype_id = "H5T_NATIVE_INT") {
  dtype_id <- h5checkConstants( "H5T", dtype_id )
  tid <- .Call("_H5Tenum_create", dtype_id, PACKAGE = "rhdf5")
}
  
#' @rdname H5T_enum
#' @export
H5Tenum_insert <- function(dtype_id, name, value) {
  
  if(!is.integer(value)) {
    stop("The 'value' argument must be an integer.")
  }
  
  res <- .Call("_H5Tenum_insert", dtype_id, name, value)
  
  return(invisible(res >= 0))
}
