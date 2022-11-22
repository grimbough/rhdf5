#' Add the BZIP2 filter to the chunk processing pipeline.
#' 
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#' creation property list.
#' @param level Compression level to be used by the selected algorithm.
#' 
#' @export
H5Pset_bzip2 <- function( h5plist, level = 2L ) {
    
    if(!is.loaded('_H5Pset_bzip2', PACKAGE = 'rhdf5'))
        stop('BZIP2 filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_bzip2", h5plist@ID, as.integer(level), PACKAGE='rhdf5')
    invisible(res)
}

#' Add the BLOSC filter to the chunk processing pipeline.
#' 
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#' creation property list.
#' @param h5tid HDF5 data type id
#' @param method Integer defining which of the compression algorithms provided
#' by BLOSC should be used. (See the details section for the mapping between 
#' integers and algorithms).
#' @param level Compression level to be used by the selected algorithm.
#' @param shuffle Logical defining whether the bit-shuffle algorithm should 
#' be used prior to compression.  This makes use of the shuffle implementation
#' provide by BLOSC, rather than the HDF5 version.
#' 
#' @export
H5Pset_blosc <- function( h5plist, h5tid, method = 1L, level = 6L, shuffle = TRUE ) {
    
    if(!is.loaded('_H5Pset_blosc', PACKAGE = 'rhdf5'))
        stop('BLOSC filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    method <- as.integer(method)
    if(method < 1 || method > 6) {
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

#' Add the LZF filter to the chunk processing pipeline.
#' 
#' @param h5plist Object of class [H5IdComponent-class] representing a dataset
#' creation property list.
#' @param h5tid HDF5 data type id
#' 
#' @export
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

#' @export
H5Pset_rle <- function( h5plist, bit32 = FALSE ) {
    
    if(!is.loaded('_H5Pset_rle', PACKAGE = 'rhdf5'))
        stop('RLE filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_rle", h5plist@ID, as.integer(bit32), PACKAGE='rhdf5')
    invisible(res)
}

#' @export
H5Pset_turborle <- function( h5plist ) {
  
  if(!is.loaded('_H5Pset_turborle', PACKAGE = 'rhdf5'))
    stop('Turbo RLE filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
  
  h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
  res <- .Call("_H5Pset_turborle", h5plist@ID, PACKAGE='rhdf5')
  invisible(res)
}

#' @export
H5Pset_lip <- function( h5plist, nbits = NULL ) {
    
    if(!is.loaded('_H5Pset_lip', PACKAGE = 'rhdf5'))
        stop('RLE filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    if(is.null(nbits)) {
        nbits <- 0L
    }
    
    if(nbits < 0 || nbits > 32) {
        stop("nbits must be between 0 and 32")
    }
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_lip", h5plist@ID, as.integer(nbits), PACKAGE='rhdf5')
    invisible(res)
}

#' @export
H5Pset_simdcomp <- function( h5plist, nbits = NULL ) {
    
    if(!is.loaded('_H5Pset_simdcomp', PACKAGE = 'rhdf5'))
        stop('RLE filter not found.\nPlease install rhdf5filters, and then reinstall rhdf5.')
    
    if(is.null(nbits)) {
        nbits <- 0L
    }
    
    if(nbits < 0 || nbits > 32) {
        stop("nbits must be between 0 and 32")
    }
    
    h5checktypeAndPLC(h5plist, "H5P_DATASET_CREATE")
    res <- .Call("_H5Pset_simdcomp", h5plist@ID, as.integer(nbits), PACKAGE='rhdf5')
    invisible(res)
}
