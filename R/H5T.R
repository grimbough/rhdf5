
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
  if (!grepl(pattern = "^[[:digit:]]+$", dtype_id))
      stop("Argument 'dtype_id' should be created by a call to H5Tcopy()")
  size <- as.integer(size)
  invisible(.Call("_H5Tset_size", dtype_id, size, PACKAGE='rhdf5'))
}

H5Tget_size <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  .Call("_H5Tget_size", dtype_id, PACKAGE='rhdf5')
}


H5Tset_strpad <- function( dtype_id, strpad = "NULLPAD") {
  
  strpad_int <- switch(strpad, 
                       NULLTERM = 0L, 
                       NULLPAD = 1L, 
                       SPACEPAD = 2L, 
                       stop("Invalid value to 'strpad' argument.\n",
                            "Valid options are: 'NULLTERM', 'NULLPAD', 'SPACEPAD'"))
  
  .Call("_H5Tset_strpad", dtype_id, strpad_int, PACKAGE = "rhdf5")
}
  
 
H5Tget_strpad <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  .Call("_H5Tget_strpad", dtype_id, PACKAGE = "rhdf5")
}

H5Tset_cset <- function( dtype_id, cset = "ASCII") {
  
  cset_int <- switch(cset, 
                     ASCII = 0L, 
                     UTF8 = 1L, 
                     stop("Invalid value to 'cset' argument.\n",
                          "Valid options are: 'ASCII', 'UTF8'")) 
  
  .Call("_H5Tset_cset", dtype_id, cset_int, PACKAGE = "rhdf5")
}
  
 
H5Tget_cset <- function( dtype_id ) {
  
  if(missing(dtype_id)) {
    stop("Argument 'dtype_id' must be supplied")
  }
  
  .Call("_H5Tget_cset", dtype_id, PACKAGE = "rhdf5")
}
