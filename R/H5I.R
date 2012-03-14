
H5Iget_type <- function( h5obj ) {
  stopifnot( is( h5obj, "H5file" ) | is( h5obj, "H5group" ) | is( h5obj, "H5dataset" ) )
  tid <- .Call("_H5Iget_type", h5obj@ID, PACKAGE='rhdf5')
  invisible(h5const2Factor("H5I_TYPE",tid))
}

