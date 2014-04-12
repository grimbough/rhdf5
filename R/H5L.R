
H5Lcreate_external <- function( target_file_name, target_obj_name, link_loc, link_name) {
  if (length(target_file_name)!=1 || !is.character(target_file_name)) stop("'target_file_name' must be a character string of length 1")
  if (length(target_obj_name)!=1 || !is.character(target_obj_name)) stop("'target_obj_name' must be a character string of length 1")
  h5checktype( link_loc, "loc")
  if (length(link_name)!=1 || !is.character(link_name)) stop("'link_name' must be a character string of length 1")

  invisible(.Call("_H5Lcreate_external", target_file_name, target_obj_name, link_loc@ID, link_name, PACKAGE='rhdf5'))
}

H5Lexists <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  name = strsplit(name,split="/")[[1]]
  name = name[nchar(name) > 0]
  Lexists = TRUE
  i=1
  while ((i <= length(name)) & (Lexists)) {
    res <- .Call("_H5Lexists", h5loc@ID, paste(name[1:i],collapse="/"), PACKAGE='rhdf5')
    Lexists <- ifelse(res > 0, TRUE, FALSE)
    i <- i + 1
  }
  Lexists
}

H5Lget_info <- function( h5loc, name ) {
  h5checktype(h5loc, "loc")
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  res <- .Call("_H5Lget_info", h5loc@ID, name, PACKAGE='rhdf5')
  res$type <- h5const2Factor("H5L_TYPE", res$type)
  res
}
