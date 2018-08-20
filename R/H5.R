
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


h5closeAll <- function() {
    
    objects <- h5validObjects()
    invisible(lapply(objects, .H5close))
}

.H5close <- function(h5id){
    
    isvalid <- H5Iis_valid(h5id)
    if (!isvalid) {
        stop("Error in h5closeAll(). H5Identifier not valid.", call. = FALSE)
    }
    
    truetype <- as.character(H5Iget_type(h5id))
    
    closeFunc <- switch(truetype,
                        H5I_FILE = H5Fclose,
                        H5I_GROUP = H5Gclose,
                        H5I_DATASET = H5Dclose,
                        H5I_GENPROP_LST = H5Pclose,
                        H5I_DATASPACE = H5Sclose,
                        H5I_ATTR = H5Aclose,
                        stop("Error in h5closeAll(). Appropriate close function not found", call. = FALSE)
    )
    
    closeFunc(h5id)
    
}