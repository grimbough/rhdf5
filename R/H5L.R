#' Create a link to an object in a different HDF5 file
#' 
#' @description `H5Lcreate_external()` creates a new external link. An external
#'   link is a soft link to an object in a different HDF5 file from the location
#'   of the link.
#'
#' @param target_file_name Name of the external HDF5 to link to
#' @param target_obj_name Path to the object in the file specified by
#'   `target_file_name` to link to.
#' @param link_loc [H5IdComponent-class] object giving the location where the
#'   new link should be created. Can represent an HDF5 file or group.
#' @param link_name Name (path) of the new link, relative to the location of
#'   `link_loc`.
#'
#' @examples
#' ## The example below creates a new HDF5 file in a temporary director, and then
#' ## links to the group "/foo" found in the file "multiple_dtypes.h5" 
#' ## distributed with the package.
#' 
#' h5File1 <- system.file("testfiles", "multiple_dtypes.h5", package="rhdf5")
#' h5File2 <- tempfile(pattern = "H5L_2_", fileext = ".h5")
#' h5createFile(h5File2)
#' 
#' ## open the new file & create a link to the group "/foo" in the original file
#' fid <- H5Fopen(h5File2)
#' H5Lcreate_external(target_file_name = h5File1, target_obj_name = "/foo", 
#'   link_loc = fid, link_name = "/external_link")
#' H5Fclose(fid)
#'
#' ## check the new file has a group called "/external_link"
#' h5ls(h5File2)
#'
#' @export
H5Lcreate_external <- function( target_file_name, target_obj_name, link_loc, link_name) {
  if (length(target_file_name)!=1 || !is.character(target_file_name)) 
    stop("'target_file_name' must be a character string of length 1")
  target_file_name = normalizePath(target_file_name,mustWork = FALSE)
  
  if (length(target_obj_name)!=1 || !is.character(target_obj_name)) 
    stop("'target_obj_name' must be a character string of length 1")
  
  h5checktype( link_loc, "loc")
  
  if (length(link_name)!=1 || !is.character(link_name)) 
    stop("'link_name' must be a character string of length 1")

  invisible(.Call("_H5Lcreate_external", target_file_name, target_obj_name, link_loc@ID, link_name, PACKAGE='rhdf5'))
}

#' Confirm existence of a link
#' 
#' @param h5loc An object of class [H5IdComponent-class] representing a H5
#'   location identifier (file or group).
#' @param name The name of the link to be checked
#' 
#' @export
H5Lexists <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) 
    stop("'name' must be a character string of length 1")

  name = strsplit(name,split="/")[[1]]
  name = name[nchar(name) > 0]
  Lexists = TRUE
  i=1
  while ((i <= length(name)) && (Lexists)) {
    res <- .Call("_H5Lexists", h5loc@ID, paste(name[1:i],collapse="/"), PACKAGE='rhdf5')
    Lexists <- ifelse(res > 0, TRUE, FALSE)
    i <- i + 1
  }
  Lexists
}

#' Find information about a link
#' 
#' @description  `H5Lget_info()` identifies the type of link specified by the the `h5loc`
#'   and `name` arguments. This is more limited than the equivalent function in
#'   the standard HDF5 library.
#'   
#' @param h5loc An object of class [H5IdComponent-class] representing a H5
#'   location identifier (file or group).
#' @param name The name of the link to be queried.
#'
#' @returns A character vector of length 1 giving the type of link.  Possible
#'   values are:
#' `H5L_TYPE_HARD`, `H5L_TYPE_SOFT`, `H5L_TYPE_EXTERNAL`, `H5L_TYPE_ERROR`
#'  
#' @export
H5Lget_info <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  res <- .Call("_H5Lget_info", h5loc@ID, name, PACKAGE='rhdf5')
  res$type <- h5const2String("H5L_TYPE", res$type)
  res
}

#' Remove a link from a group
#' 
#' @param h5loc An object of class [H5IdComponent-class] representing a H5
#'   location identifier (file or group).
#' @param name The name of the link to be deleted.
#' 
#' @examples 
#' 
#' # create an hdf5 file and a group
#' h5createFile("ex_H5L.h5")
#' h5createGroup("ex_H5L.h5","/foo")
#' 
#' # reopen file and confirm "/foo" exists but "/baa" does not
#' fid <- H5Fopen("ex_H5L.h5")
#' H5Lexists(fid, "/foo")
#' 
#' # remove the link to "/foo" and confirm it no longer exists
#' H5Ldelete(fid, "/foo")
#' H5Lexists(fid, "/foo")
#' 
#' H5Fclose(fid)
#' 
#' @export
H5Ldelete <- function( h5loc, name ) {
    
    h5checktype(h5loc, "loc")
    if ( length(name) != 1 || !is.character(name) ) {
        stop("'name' must be a character string of length 1")
    }
    
    if(!H5Lexists(h5loc, name)) {
        stop("Specified link doesn't exist.")
    }
    
    res <- .Call("_H5Ldelete", h5loc@ID, name, PACKAGE='rhdf5')
    
    if(res < 0) {
        stop('Link deletion failed')
    } else {
        return( invisible(res) )
    }
}

#' Move a link within an HDF5 file
#'
#' @param h5loc An object of class [H5IdComponent-class] representing a H5
#'   location identifier (file or group) where the new link is placed.
#' @param name The name of the link to be moved.
#' @param h5loc_dest [H5IdComponent-class] object representing the H5 location
#'   where the new link should be created.
#' @param name_dest Name of the new link to be created
#' @param lcpl,lapl Link creation and link access property lists to be
#'   associated with the new link.  Leaving these arguments as `NULL` will use
#'   the HDF5 default property lists.
#'
#' @examples
#' ## create an HDF5 file with a single group
#' ## that contains a dataset of 10 numbers
#' h5file <- tempfile(fileext = ".h5")
#' h5createFile(h5file)
#' h5createGroup(h5file, "/foo")
#' h5write(1:10, h5file, name = "/foo/vector1")
#' ## check the structure is what we expect
#' h5ls(h5file)
#'
#' ## open the file, the group where the dataset currently is
#' ## and the root group
#' fid <- H5Fopen(name = h5file)
#' gid1 <- H5Gopen(fid, "/foo")
#' gid2 <- H5Gopen(fid, "/")
#' ## move the dataset to the root of the file and rename it
#' H5Lmove(gid1, "vector1", gid2, "vector_new")
#' h5closeAll()
#' ## check the dataset has moved out of the foo group
#' h5ls(h5file)
#'
#' ## we can also provide the ID of the HDF5 file
#' ## and use the "name" arguments to move between groups
#' fid <- H5Fopen(name = h5file)
#' H5Lmove(fid, "/vector_new", fid, "/foo/vector_newer")
#' H5Fclose(fid)
#' h5ls(h5file)
#'
#' @export
H5Lmove <- function( h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL ) {
    
    h5checktype(h5loc, "loc")
    h5checktype(h5loc_dest, "loc")

    ## if not provided a creation property list, use the default   
    if(is.null(lcpl)) {
        lcpl <- H5Pcreate("H5P_LINK_CREATE")
        on.exit(H5Pclose(lcpl), add = TRUE)
    } else {
        lcpl <- h5checktypeAndPLC(lcpl, "H5P_LINK_CREATE", allowNULL = FALSE)
    }
    
    ## use default access property list if not given
    if(is.null(lapl)) {
        lapl <- H5Pcreate("H5P_LINK_ACCESS")
        on.exit(H5Pclose(lapl), add = TRUE)
    } else {
        lapl <- h5checktypeAndPLC(lapl, "H5P_LINK_ACCESS", allowNULL = FALSE) 
    }
    
    res <- .Call("_H5Lmove", h5loc@ID, name, h5loc_dest@ID, name_dest, lcpl, lapl, PACKAGE='rhdf5')
    
    if(res < 0) {
        stop('Link deletion failed')
    } else {
        return( invisible(res) )
    }
  
}

#' Copy a link from one location to another
#' 
#' @param h5loc An object of class [H5IdComponent-class] representing a H5 
#' location identifier (file or group) where the new link is placed.
#' @param name The name of the link to be copied.
#' @param h5loc_dest An object of class [H5IdComponent-class] representing
#' the destination file or group where a copied or moved link should be created.
#' @param name_dest The name of the link to be created when copying or moving.
#' @param lcpl,lapl Link creation and link access property lists.  If left as
#' `NULL` the HDF5 defaults will be used.
#' 
#' @export
H5Lcopy <- function( h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL ) {
    
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
    if(is.null(lapl)) {
        lapl <- H5Pcreate("H5P_LINK_ACCESS")
        on.exit(H5Pclose(lapl), add = TRUE)
    } else {
        lapl <- h5checktypeAndPLC(lapl, "H5P_LINK_ACCESS", allowNULL = FALSE) 
    }
    
    res <- .Call("_H5Lcopy", h5loc@ID, name, h5loc_dest@ID, name_dest, lcpl, lapl, PACKAGE='rhdf5')
    
    if(res < 0) {
        stop('Link deletion failed')
    } else {
        return( invisible(res) )
    }
    
}
