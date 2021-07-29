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

#' Create a hard link to an object in an HDF5 file
#' 
#' @param h5obj An object of class [H5IdComponent-class] representing the object to be linked to.
#' @param h5loc An object of class [H5IdComponent-class] representing the location at which the 
#' object is to be linked.  Can represent a file, group, dataset, datatype or attribute.
#' @param newLinkName Character string giving the name of the new link.  This should be relative to
#' `h5loc`.
#' @param lcpl,lapl [H5IdComponent-class] objects representing link creation and link access property lists
#' respectively.  If left as `NULL` the default values for these will be used.
#' 
#' @seealso [H5Gcreate_anon]
#' 
#' @examples 
#' ## Create a temporary copy of an example file, and open it
#' example_file <- system.file("testfiles", "h5ex_t_array.h5", package="rhdf5")
#' file.copy(example_file, tempdir())
#' h5_file <- file.path(tempdir(), "h5ex_t_array.h5")
#' fid <- H5Fopen( h5_file )
#' 
#' ## create a new group without a location in the file
#' gid <- H5Gcreate_anon(fid)
#' 
#' ## create link to newly create group
#' ## relative to the file identifier
#' H5Olink(h5obj = gid, h5loc = fid, newLinkName = "foo")
#' 
#' ## tidy up
#' H5Gclose(gid)
#' H5Fclose(fid)
#' 
#' ## Check we now have a "/foo" group
#' h5ls( h5_file )
#' 
#' @export
H5Olink <- function( h5obj, h5loc, newLinkName, lcpl = NULL, lapl = NULL ) {
  h5checktype(h5obj, "object")
  h5checktype(h5loc, "loc")
  
  lcpl <- h5checktypeAndPLC(lcpl, "H5P_LINK_CREATE", allowNULL = TRUE)
  lapl <- h5checktypeAndPLC(lapl, "H5P_LINK_ACCESS", allowNULL = TRUE)
  
  res <- .Call("_H5Olink", h5obj@ID, h5loc@ID, newLinkName, lcpl, lapl, PACKAGE='rhdf5')
  
  if(res < 0) {
    stop("Link creation failed")
  } else {
    return(invisible(TRUE))
  }
}

