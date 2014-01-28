
H5open <- function( ) {
  invisible(.Call("_H5open", PACKAGE='rhdf5'))
}

H5close <- function( ) {
  invisible(.Call("_H5close", PACKAGE='rhdf5'))
}

H5garbage_collect <- function( ) {
  invisible(.Call("_H5garbage_collect", PACKAGE='rhdf5'))
}

H5get_libversion <- function( ) {
  .Call("_H5get_libversion", PACKAGE='rhdf5')
}

