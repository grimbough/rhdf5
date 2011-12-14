
H5OconvertTypes <- function(L) {
  L$type <- h5const2Factor("H5O_TYPE", L$type)
  L$atime <- .POSIXct(L$atime)
  L$atime[L$atime == 0] <- NA
  L$mtime <- .POSIXct(L$mtime)
  L$mtime[L$mtime == 0] <- NA
  L$ctime <- .POSIXct(L$ctime)
  L$ctime[L$ctime == 0] <- NA
  L$btime <- .POSIXct(L$btime)
  L$btime[L$btime == 0] <- NA
  L
}

H5Oget_info <- function( h5obj ) {
  stopifnot( is( h5obj, "H5file" ) | is( h5obj, "H5group" ) | is( h5obj, "H5dataset" ))

  res <- .Call("_H5Oget_info", h5obj@ID, PACKAGE='rhdf5')
  res <- H5OconvertTypes(res)
  res
}

H5Oget_info_by_name <- function( h5loc, name ) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

  res <- .Call("_H5Oget_info_by_name", h5loc@ID, name, PACKAGE='rhdf5')
  res <- H5OconvertTypes(res)
  res
}
