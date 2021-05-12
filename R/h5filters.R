#' Identifies the filters required to read a dataset
#' If filters aren't available it will try to identify them
#' and print the names to the user.
#' @keywords internal
h5checkFilters <- function(h5id) {
    
    truetype = H5Iget_type(h5id)
    
    if(truetype == "H5I_DATASET") {
        
        pid <- H5Dget_create_plist(h5id)
        on.exit(H5Pclose(pid))
        
    } else if(truetype == "H5I_GENPROP_LST") {
        ## we require this to be a DATASET creation plist
        pid <- h5checktypeAndPLC(h5id, "H5P_DATASET_CREATE")
    }
    
    nfilters = H5Pget_nfilters(pid)
    if( (nfilters > 0) && (H5Pall_filters_avail(pid) == 0) ) {
        
        err <-"Unable to read dataset.\nNot all required filters available.\n"
        missing <- NULL
        for(i in seq_len( nfilters )) {
            filter <- H5Pget_filter(pid, i-1)
            avail <- H5Zfilter_avail(filter_id = filter[[1]])
            if(!avail) {
                missing <- c(missing, filter[[2]])
            }
        }
        err <- paste0(err, "Missing filters: ", paste(missing, collapse = " "))
        return(err)   
    } else {
        return("")
    }
}

H5Pset_bzip2 <- function( h5plist, level = 2L ) {
    
    if(!is.loaded('_H5Pset_bzip2', PACKAGE = 'rhdf5'))
        stop('BZIP2 filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_bzip2", h5plist@ID, as.integer(level), PACKAGE='rhdf5')
    invisible(res)
}

H5Pset_blosc <- function( h5plist, h5tid, method = 1L, level = 6L, shuffle = TRUE ) {
    
    if(!is.loaded('_H5Pset_blosc', PACKAGE = 'rhdf5'))
        stop('BLOSC filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    if(!method %in% 1:6) {
        method <- 1L
        warning('Invalid method selected. Using BLOSC_LZ')
    }
    
    ## START: simplified reimplementation of C code from H5Zblosc.c
    ## Filter from https://github.com/nexusformat/HDF5-External-Filter-Plugins/
    ## contains a call to blosc_set_local() which determines chunk size & blosc
    ## parameters. This requires calls to HDF5 functions and doesn't play well
    ## with our static linking.  We move this setup code into R code below.
    chunkdims <- H5Pget_chunk(h5plist)
    typesize <- H5Tget_size(h5tid)
    if(typesize > 255) { typesize <- 1 }
    bufsize <- typesize * prod(chunkdims)
    ## END
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_blosc", h5plist@ID, 
                 as.integer(method-1L), as.integer(level), 
                 as.integer(as.logical(shuffle)),
                 as.integer(typesize), as.integer(bufsize),
                 PACKAGE='rhdf5')
    invisible(res)
}

H5Pset_lzf <- function( h5plist, h5tid ) {
    
    if(!is.loaded('_H5Pset_lzf', PACKAGE = 'rhdf5'))
        stop('LZF filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')

    ## START: simplified reimplementation of C code from lzf_filter.c
    ## Filter from h5py
    ## contains a call to lzf_set_local() which determines chunk size
    ## This requires calls to HDF5 functions and doesn't play well
    ## with our static linking.  We move this setup code into R code below.
    chunkdims <- H5Pget_chunk(h5plist)
    typesize <- H5Tget_size(h5tid)
    bufsize <- typesize * prod(chunkdims)
    ## END
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_lzf", h5plist@ID, as.integer(bufsize), PACKAGE='rhdf5')
    invisible(res)
}