#' Create a new HDF5 group and link it to a location in a file
#' 
#' `H5Gcreate` is used to a new group and link it into a file.
#' 
#' @param h5loc An object of class [H5IdComponent-class]
#' @param name Name of the new group to be created.
#' 
#' @export
H5Gcreate <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  gid <- .Call("_H5Gcreate", h5loc@ID, name, PACKAGE='rhdf5')
  if (gid > 0) {
    h5group = new("H5IdComponent", ID = gid, native = h5loc@native)
  } else {
    message("HDF5: unable to create group")
    h5group = FALSE
  }
  invisible(h5group)
}

#' Create a new HDF5 group without linking it into a file
#' 
#' @param h5loc An object of class [H5IdComponent-class] specifying the file in
#' which the new group is to be created.
#' 
#' @return `H5Gcreate_anon` returns an object of class [H5IdComponent-class] 
#' representing the newly created group.  However at this point is is still
#' anonymous, and must be linked into the file structure via [H5Olink()].  
#' If this is not done, the group will be deleted from the file when it
#' is closed.
#' 
#' @seealso [H5Gcreate()], [H5Olink()]
#' @export
H5Gcreate_anon <- function( h5loc ) {
  h5checktype(h5loc, "loc")
  gid <- .Call("_H5Gcreate_anon", h5loc@ID, PACKAGE='rhdf5')
  if (gid > 0) {
    h5group = new("H5IdComponent", ID = gid, native = h5loc@native)
  } else {
    message("HDF5: unable to create group")
    h5group = FALSE
  }
  invisible(h5group)
}

#' Open a specified group
#' 
#' @param h5loc An object of class [H5IdComponent-class] representing a H5 
#' file or group that contains the group to be opened.
#' @param name Name of the group to open.
#' 
#' @return An object of class [H5IdComponent-class] representing the opened 
#' group.  When access to the group is no longer needed this should be released
#' with [H5Gclose()] to prevent resource leakage.
#' 
#' @seealso [H5Gclose()]
#' 
#' @export
H5Gopen <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  gid <- .Call("_H5Gopen", h5loc@ID, name, PACKAGE='rhdf5')
  if (gid > 0) {
    h5group = new("H5IdComponent", ID = gid, native = h5loc@native)
  } else {
    message("HDF5: unable to open group")
    h5group = FALSE
  }
  invisible(h5group)
}

#' Close a specified group
#' 
#' @param h5group An object of class [H5IdComponent-class] representing a H5 
#' group.  Typically created via [H5Gopen()] or [H5Gcreate()].
#' 
#' @export
H5Gclose <- function( h5group ) {
  h5checktype(h5group, "group")
  invisible(.Call("_H5Gclose", h5group@ID, PACKAGE='rhdf5'))
}

#' Retrieve information about a group
#' 
#' @param h5loc An object of class [H5IdComponent-class] representing a H5 
#' group.
#' @param group_name An additional group name specifying the group for which 
#' information is sought. It is interpreted relative to `h5loc`.
#' @param n Position in the index of the group for which information is 
#' retrieved.
#' @param index_type See \code{h5const("H5_INDEX")} for possible arguments.
#' @param order See \code{h5const("H5_ITER")} for possible arguments.
#' 
#' @return A list with group information
#' 
#' @examples 
#' h5file <- system.file("testfiles", "multiple_dtypes.h5", package="rhdf5")
#' fid <- H5Fopen(h5file)
#' gid <- H5Gopen(fid, "/foo")
#' gid
#' H5Gget_info(gid)
#' H5Gclose(gid)
#' 
#' ## the "get_info_by" functions take the H5 object that contains the
#' ## group(s) of interest.  We can retrieve information by index or by name
#' H5Gget_info_by_idx(fid, 3)
#' H5Gget_info_by_name(fid,"/foo")
#' 
#' H5Fclose(fid)
#' 
#' @name H5Gget_info
NULL

#' @rdname H5Gget_info
#' @export
H5Gget_info <- function( h5loc ) {
  h5checktype(h5loc, "loc")
  .Call("_H5Gget_info", h5loc@ID, PACKAGE='rhdf5')
}

#' @rdname H5Gget_info
#' @export
H5Gget_info_by_name <- function( h5loc, group_name ) {
  h5checktype(h5loc, "loc")
  if (length(group_name)!=1 || !is.character(group_name)) stop("'group_name' must be a character string of length 1")
  .Call("_H5Gget_info_by_name", h5loc@ID, group_name, PACKAGE='rhdf5')
}

#' @rdname H5Gget_info
#' @export
H5Gget_info_by_idx <- function( h5loc, n, group_name=".", index_type = h5default("H5_INDEX"), order = h5default("H5_ITER")) {
  h5checktype(h5loc, "loc")
  if (length(n)!=1) stop("'n' must be an integer of length 1")
  n <- as.integer(n) - 1L  # we will use R style numbering beginning with 1
  if (n < 0) stop("'n' must be larger than 1")
  if (length(group_name)!=1 || !is.character(group_name)) stop("'group_name' must be a character string of length 1")
  index_type <- h5checkConstants( "H5_INDEX", index_type )
  order <- h5checkConstants( "H5_ITER", order )
  .Call("_H5Gget_info_by_idx", h5loc@ID, group_name, index_type, order, n, PACKAGE='rhdf5')
}
