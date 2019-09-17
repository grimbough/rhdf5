
H5Tcopy <- function( dtype_id = h5default(type="H5T")) {
  if (is.numeric(dtype_id)) {
    dtype_id <- as.integer(dtype_id)
  } else {
    dtype_id <- h5checkConstants( "H5T", dtype_id )
  }
  invisible(.Call("_H5Tcopy", dtype_id, PACKAGE='rhdf5'))
}

H5Tset_size <- function( dtype_id = h5default(type="H5T"), size) {
  # string constant type_id do not make sense, because they are not allowed to be changed
  ## dtype_id <- as.integer(dtype_id)
  if (!grepl(pattern = "^[[:digit:]]+$", dtype_id))
      stop("Argument 'dtype_id' should be created by a call to H5Tcopy()")
  size <- as.integer(size)
  invisible(.Call("_H5Tset_size", dtype_id, size, PACKAGE='rhdf5'))
}

## not currently exported - MLS 23-01-2019
H5Tget_size <- function( dtype_id = h5default(type="H5T")) {
  .Call("_H5Tget_size", dtype_id, PACKAGE='rhdf5')
}
