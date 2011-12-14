
H5Lexists <- function( h5loc, name ) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
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
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  res <- .Call("_H5Lget_info", h5loc@ID, name, PACKAGE='rhdf5')
  res$type <- h5const2Factor("H5L_TYPE", res$type)
  res
}
