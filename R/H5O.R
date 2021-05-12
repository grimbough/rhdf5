#' Open an object in an HDF5 file
#' 
#' @param h5loc An object of class [H5IdComponent-class]
#' @param name Path to the object to be opened.  This should be relative to
#' `h5loc` rather than the file.
#' 
#' @return An object of class [H5IdComponent-class] if the open operation was
#' successful. `FALSE` otherwise.
#' 
#' @examples
#' 
#' # create an hdf5 file and write something
#' h5createFile("ex_H5O.h5")
#' h5createGroup("ex_H5O.h5","foo")
#' B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
#' h5write(B, "ex_H5O.h5","foo/B")
#' 
#' # reopen file and dataset and get object info
#' fid <- H5Fopen("ex_H5O.h5")
#' oid = H5Oopen(fid, "foo")
#' H5Oget_num_attrs(oid)
#' H5Oclose(oid)
#' H5Fclose(fid)
#' 
#' @seealso [H5Oclose()]
#' 
#' @export
H5Oopen <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  oid <- .Call("_H5Oopen", h5loc@ID, name, PACKAGE='rhdf5')
  if (oid > 0) {
    h5object = new("H5IdComponent", ID = oid, native = h5loc@native)
  } else {
    message("HDF5: unable to open object")
    h5object = FALSE
  }
  invisible(h5object)
}

#' Close an HDF5 object
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing an open
#' HDF5 object.
#' 
#' @seealso [H5Oopen()]
#' 
#' @export
H5Oclose <- function( h5obj ) {
  h5checktype(h5obj, "object")
  invisible(.Call("_H5Oclose", h5obj@ID, PACKAGE='rhdf5'))
}

#' Find the number of attributes associated with an HDF5 object
#' 
#' @details These functions are not part of the standard HDF5 C API.
#' 
#' @return Returns a vector of length 1 containing the number of attributes 
#' the specified object has.
#' 
#' @name H5Oget_num_attrs
NULL

#' @rdname H5Oget_num_attrs
#' @export
H5Oget_num_attrs <- function( h5obj ) {
  h5checktype(h5obj, "object")
  n <- .Call("_H5Oget_num_attrs", h5obj@ID, PACKAGE='rhdf5')
  n
}

#' @rdname H5Oget_num_attrs
#' @export
H5Oget_num_attrs_by_name <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  h5obj = H5Oopen( h5loc, name )
  n <- .Call("_H5Oget_num_attrs", h5obj@ID, PACKAGE='rhdf5')
  H5Oclose(h5obj);
  n
}

