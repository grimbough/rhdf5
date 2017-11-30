
h5lsConvertToDataframe <- function(L, all=FALSE) {
  if (is.data.frame(L)) {
    L$ltype <- h5const2Factor("H5L_TYPE", L$ltype)
    L$otype <- h5const2Factor("H5I_TYPE", L$otype)
#    L$atime <- .POSIXct(L$atime)
#    L$atime[L$atime == 0] <- NA
#    L$mtime <- .POSIXct(L$mtime)
#    L$mtime[L$mtime == 0] <- NA
#    L$ctime <- .POSIXct(L$ctime)
#    L$ctime[L$ctime == 0] <- NA
#    L$btime <- .POSIXct(L$btime)
#    L$btime[L$btime == 0] <- NA
    ## L <- as.data.frame(L, stringsAsFactors=FALSE)
    if (!all) {
      L <- L[,c("group", "name", "otype", "dclass","dim")]
    }
  } else {
    for (i in seq_len(length(L))) {
      L[i] <- list(h5lsConvertToDataframe(L[[i]],all=all))
    }
  }
  L
}

h5ls <- function( file, recursive = TRUE, all=FALSE, datasetinfo=TRUE, index_type = h5default("H5_INDEX"), order = h5default("H5_ITER")) {
    
    loc = h5checktypeOrOpenLoc(file, readonly=TRUE)
    on.exit(h5closeitLoc(loc))
    
    if (length(datasetinfo)!=1 || !is.logical(datasetinfo)) stop("'datasetinfo' must be a logical of length 1")
    index_type <- h5checkConstants( "H5_INDEX", index_type )
    order <- h5checkConstants( "H5_ITER", order )
    if (is.logical(recursive)) {
        if (recursive) {
            depth = -1L
        } else {
            depth = 1L
        }
    } else if ( is.numeric(recursive) | is.integer(recursive) ) {
        depth = as.integer(recursive)
        if( length(recursive) > 1 ) {
            warning("'recursive' must be of length 1.  Only using first value.")
        } else if (recursive == 0) {
            stop("value 0 for 'recursive' is undefined, either a positive integer or negative (maximum recursion)")
        } 
    } else {
        stop("'recursive' must be number or a logical")
    }
    di <- ifelse(datasetinfo, 1L, 0L)
    L <- .Call("_h5ls", loc$H5Identifier@ID, depth, di, index_type, order, PACKAGE='rhdf5')
    L <- h5lsConvertToDataframe(L, all=all)
    L
}


