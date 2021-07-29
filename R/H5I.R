
#' Retrieve the name of an object from a given identifier
#' 
#' @param h5obj An object of class [H5IdComponent-class].  Can represent a 
#' file, group, dataset or attribute.
#' 
#' @export
H5Iget_name <- function( h5obj ) {
  h5checktype(h5obj, "object")
  name <- .Call("_H5Iget_name", h5obj@ID, PACKAGE='rhdf5')
  name
}

#' Find the type of an object
#' 
#' Possible types returned by the function are:
#' * `H5I_FILE`
#' * `H5I_GROUP`
#' * `H5I_DATATYPE`
#' * `H5I_DATASPACE`
#' * `H5I_DATASET`
#' * `H5I_ATTR`
#' 
#' @return Returns a character vector of length 1 containing the HDF5 type
#' for the supplied identifier.
#' 
#' @param h5identifier Object of class [H5IdComponent-class].
#' 
#' @examples 
#' h5file <- system.file("testfiles", "h5ex_t_array.h5", package="rhdf5")
#' fid <- H5Fopen(h5file)
#' gid <- H5Gopen(fid, "/")
#' 
#' ## identify the HDF5 types for these identifiers
#' H5Iget_type(fid)
#' H5Iget_type(gid)
#' 
#' ## tidy up
#' H5Gclose(gid)
#' H5Fclose(fid)
#' 
#' @export
H5Iget_type <- function( h5identifier ) {
  stopifnot( is( h5identifier, "H5IdComponent" ) )
  tid <- .Call("_H5Iget_type", h5identifier@ID, PACKAGE='rhdf5')
  h5const2String("H5I_TYPE",tid)
}

#' Determine whether an identifier is valid
#' 
#' An identifier is no longer valid after it has been closed.
#' 
#' @param h5identifier Object of class [H5IdComponent-class].
#' 
#' @return A logical of length 1.  `TRUE` is the identifier is valid,
#' `FALSE` if not.
#' 
#' @examples
#' 
#' h5file <- system.file("testfiles", "h5ex_t_array.h5", package="rhdf5")
#' fid <- H5Fopen(h5file)
#' 
#' ## test whether the identifer to the opened file is valid
#' H5Iis_valid(fid)
#' 
#' ## the file ID is no longer valid after it has been closed
#' H5Fclose(fid)
#' H5Iis_valid(fid)
#' 
#' @export
H5Iis_valid<- function( h5identifier ) {
  stopifnot( is( h5identifier, "H5IdComponent" ) )
  tid <- .Call("_H5Iis_valid", h5identifier@ID, PACKAGE='rhdf5')
  tid
}

