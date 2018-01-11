
H5Gcreate <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  gid <- .Call("_H5Gcreate", h5loc@ID, name, PACKAGE='rhdf5')
  if (gid > 0) {
    h5group = new("H5IdComponent", ID = gid, native = .native(h5loc))
  } else {
    message("HDF5: unable to create group")
    h5group = FALSE
  }
  invisible(h5group)
}

H5Gcreate_anon <- function( h5loc ) {
  h5checktype(h5loc, "loc")
  gid <- .Call("_H5Gcreate_anon", h5loc@ID, PACKAGE='rhdf5')
  if (gid > 0) {
    h5group = new("H5IdComponent", ID = gid, native = .native(h5loc))
  } else {
    message("HDF5: unable to create group")
    h5group = FALSE
  }
  invisible(h5group)
}

H5Gopen <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  gid <- .Call("_H5Gopen", h5loc@ID, name, PACKAGE='rhdf5')
  if (gid > 0) {
    h5group = new("H5IdComponent", ID = gid, native = .native(h5loc))
  } else {
    message("HDF5: unable to open group")
    h5group = FALSE
  }
  invisible(h5group)
}

H5Gclose <- function( h5group ) {
  h5checktype(h5group, "group")
  invisible(.Call("_H5Gclose", h5group@ID, PACKAGE='rhdf5'))
}

H5Gget_info <- function( h5loc ) {
  h5checktype(h5loc, "loc")
  .Call("_H5Gget_info", h5loc@ID, PACKAGE='rhdf5')
}

H5Gget_info_by_name <- function( h5loc, group_name ) {
  h5checktype(h5loc, "loc")
  if (length(group_name)!=1 || !is.character(group_name)) stop("'group_name' must be a character string of length 1")
  .Call("_H5Gget_info_by_name", h5loc@ID, group_name, PACKAGE='rhdf5')
}

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
