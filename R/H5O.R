

#H5OconvertTypes <- function(L) {
#  L$type <- h5const2Factor("H5O_TYPE", L$type)
#  L$atime <- .POSIXct(L$atime)
#  L$atime[L$atime == 0] <- NA
#  L$mtime <- .POSIXct(L$mtime)
#  L$mtime[L$mtime == 0] <- NA
#  L$ctime <- .POSIXct(L$ctime)
#  L$ctime[L$ctime == 0] <- NA
#  L$btime <- .POSIXct(L$btime)
#  L$btime[L$btime == 0] <- NA
#  L
#}

H5Oopen <- function( h5loc, name ) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  oid <- .Call("_H5Oopen", h5loc@ID, name, PACKAGE='rhdf5')
  if (oid > 0) {
    h5object = new("H5group", ID = oid)
  } else {
    message("HDF5: unable to open object")
    h5object = FALSE
  }
  invisible(h5object)
}

H5Oclose <- function( h5obj ) {
  stopifnot( is( h5obj, "H5group" ) )
  invisible(.Call("_H5Oclose", h5obj@ID, PACKAGE='rhdf5'))
}

H5Oget_num_attrs <- function( h5obj ) {
  stopifnot( is( h5obj, "H5file" ) | is( h5obj, "H5group" ) | is( h5obj, "H5dataset" ) )
  n <- .Call("_H5Oget_num_attrs", h5obj@ID, PACKAGE='rhdf5')
  n
}

H5Oget_num_attrs_by_name <- function( h5loc, name ) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")
  h5obj = H5Oopen( h5loc, name )
  n <- .Call("_H5Oget_num_attrs", h5obj@ID, PACKAGE='rhdf5')
  H5Oclose(h5obj);
  n
}


#H5Oget_info <- function( h5obj ) {
#  stopifnot( is( h5obj, "H5file" ) | is( h5obj, "H5group" ) | is( h5obj, "H5dataset" ))

#  res <- .Call("_H5Oget_info", h5obj@ID, PACKAGE='rhdf5')
#  res <- H5OconvertTypes(res)
#  res
#}

#H5Oget_info_by_name <- function( h5loc, name ) {
#  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) )
#  if (length(name)!=1 || !is.character(name)) stop("'name' must be a character string of length 1")

#  res <- .Call("_H5Oget_info_by_name", h5loc@ID, name, PACKAGE='rhdf5')
#print("returned from C")
#  res <- H5OconvertTypes(res)
#  res
#}
