#' Find the number of attributes associated with an HDF5 object
#' 
#' @details These functions are not part of the standard HDF5 C API.
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing a H5 object identifier (file, group, or dataset).
#' @param h5loc An object of class [H5IdComponent-class] representing a H5 location identifier (file or group).
#' @param name The name of the object to be checked.
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
