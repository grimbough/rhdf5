
H5Iget_name <- function( h5obj ) {
  h5checktype(h5obj, "object")
  tid <- .Call("_H5Iget_name", h5obj@ID, PACKAGE='rhdf5')
  tid
}

H5Iget_type <- function( h5identifier ) {
  stopifnot( is( h5identifier, "H5IdComponent" ) )
  tid <- .Call("_H5Iget_type", h5identifier@ID, PACKAGE='rhdf5')
  h5const2Factor("H5I_TYPE",tid)
}

H5Iis_valid<- function( h5identifier ) {
  stopifnot( is( h5identifier, "H5IdComponent" ) )
  tid <- .Call("_H5Iis_valid", h5identifier@ID, PACKAGE='rhdf5')
  tid
}

