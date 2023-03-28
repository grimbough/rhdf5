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
#' h5File <- tempfile(pattern = "ex_H5O.h5")
#' 
#' # create an hdf5 file and write something
#' h5createFile(h5File)
#' h5createGroup(h5File,"foo")
#' B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
#' h5write(B, h5File,"foo/B")
#' 
#' # reopen file and dataset and get object info
#' fid <- H5Fopen(h5File)
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

#' Copies an HDF5 object
#' 
#' @param h5loc An object of class [H5IdComponent-class] representing an open
#' HDF5 object where the source obiect should be copied from.
#' @param name Character vector of length 1, giving the name of the source object
#' to be copied.
#' @param h5loc_dest An object of class [H5IdComponent-class] representing an open
#' HDF5 object where the new copy should be created.
#' @param name_dest Character vector of length 1, giving the name of the new object
#' to be created.
#' @param obj_cpy_pl,lcpl [H5IdComponent-class] objects representing object copy and link creation property lists
#' respectively.  If left as `NULL` the default values for these will be used.
#' 
#' @examples 
#' 
#' ## Create a temporary copy of an example file check the contents
#' example_file <- system.file("testfiles", "h5ex_t_array.h5", package="rhdf5")
#' file.copy(example_file, tempdir())
#' h5_file <- file.path(tempdir(), "h5ex_t_array.h5")
#' h5ls(h5_file)
#' 
#' ## open the example file and create a new, empty, file
#' fid1 <- H5Fopen( h5_file )
#' h5_file2 <- tempfile(fileext = ".h5")
#' fid2 <- H5Fcreate( h5_file2 )
#' 
#' ## We can copy a dataset inside the same file
#' H5Ocopy(h5loc = fid1, name = "DS1", h5loc_dest = fid1, name_dest = "DS2")
#' ## Or to a different file
#' H5Ocopy(h5loc = fid1, name = "DS1", h5loc_dest = fid2, name_dest = "DS1_copy")
#' 
#' ## if we want to create a new group hierarchy we can use a link creation property list
#' lcpl <- H5Pcreate("H5P_LINK_CREATE")
#' H5Pset_create_intermediate_group( lcpl, create_groups = TRUE )
#' H5Ocopy(h5loc = fid1, name = "DS1", h5loc_dest = fid2, 
#'         name_dest = "/foo/baa/DS1_nested", lcpl = lcpl)
#' 
#' ## tidy up
#' H5Pclose(lcpl)
#' H5Fclose(fid1)
#' H5Fclose(fid2)
#' 
#' ## Check we now have groups DS1 and DS2 in the original file
#' h5ls( h5_file )
#' ## Check we have a copy of DS1 at the root and nests in the new file
#' h5ls( h5_file2 )
#' 
#' @export
H5Ocopy <- function( h5loc, name, h5loc_dest, name_dest, obj_cpy_pl = NULL, lcpl = NULL ) {
  h5checktype(h5loc, "loc")
  h5checktype(h5loc_dest, "loc")
  
  ## if not provided a creation property list, use the one from the source    
  if(is.null(lcpl)) {
    lcpl <- H5Pcreate("H5P_LINK_CREATE")
    on.exit(H5Pclose(lcpl), add = TRUE)
  } else {
    lcpl <- h5checktypeAndPLC(lcpl, "H5P_LINK_CREATE", allowNULL = FALSE)
  }
  
  ## use default access property list if not given
  if(is.null(obj_cpy_pl)) {
    obj_cpy_pl <- H5Pcreate("H5P_OBJECT_COPY")
    on.exit(H5Pclose(obj_cpy_pl), add = TRUE)
  } else {
    lapl <- h5checktypeAndPLC(lapl, "H5P_OBJECT_COPY", allowNULL = FALSE) 
  }
  
  res <- invisible(.Call("_H5Ocopy", h5loc@ID, name, h5loc_dest@ID, name_dest, obj_cpy_pl@ID, lcpl@ID, PACKAGE='rhdf5'))
  
  if(res < 0) {
    stop("Object copying failed")
  } else {
    return(invisible(TRUE))
  }
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
  
  res <- .Call("_H5Olink", h5obj@ID, h5loc@ID, newLinkName, lcpl@ID, lapl@ID, PACKAGE='rhdf5')
  
  if(res < 0) {
    stop("Link creation failed")
  } else {
    return(invisible(TRUE))
  }
}

