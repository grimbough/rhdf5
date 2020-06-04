
h5loadData <- function(h5loc, L, all=FALSE, ..., native) {

    h5checktype(h5loc,"loc")
    if (length(L) > 0) {
        for (i in seq_len(length(L))) {
            if (is.data.frame(L[[i]])) {
                if (L[[i]]$ltype %in% h5constants[["H5L_TYPE"]][c("H5L_TYPE_HARD","H5L_TYPE_EXTERNAL")]) {
                    if (L[[i]]$otype == h5constants[["H5I_TYPE"]]["H5I_DATASET"]) {
                        L[i] = list(h5read(
                            h5loc, L[[i]]$name, ..., native = native
                        ))
                    } else {
                        L[i] = h5lsConvertToDataframe(
                            L[i], all=all, native=native
                        )
                    }
                } else {
                    L[i] = h5lsConvertToDataframe(L[i], all=all, native=native)
                }
            } else {
                group = H5Gopen(h5loc, names(L)[i])
                L[i] = list(
                    h5loadData(group, L[[i]], all=all, ..., native = native)
                )
                H5Gclose(group)
            }
        }
    }
    L
}

h5dump <- function( file, recursive = TRUE, load=TRUE, all=FALSE, 
                    index_type = h5default("H5_INDEX"), order = h5default("H5_ITER"), 
                    s3 = FALSE, s3credentials = NULL, ..., native=FALSE) {

    if(isTRUE(s3)) {
        fapl <- H5Pcreate("H5P_FILE_ACCESS")
        on.exit(H5Pclose(fapl))
        H5Pset_fapl_ros3(fapl, s3credentials)
        loc <- h5checktypeOrOpenLocS3(file, readonly = TRUE, fapl = fapl, native = native)
    } else {
        loc <- h5checktypeOrOpenLoc(file, readonly = TRUE, fapl = NULL, native = native)
    }
    on.exit(h5closeitLoc(loc), add = TRUE)
    
    index_type <- h5checkConstants( "H5_INDEX", index_type )
    order <- h5checkConstants( "H5_ITER", order )
    if (is.logical(recursive)) {
        if (recursive) {
            depth = -1L
        } else {
            depth = 1L
        }
    } else if (is.numeric(recursive) | is.integer(recursive) ) {
        depth = as.integer(recursive)
        if( length(recursive) > 1 ) {
            warning("'recursive' must be of length 1.  Only using first value.")
        } else if (recursive == 0) {
            stop("value 0 for 'recursive' is undefined, either a positive number specify the depth to descend or negative for maximum recursion")
        }
    } else {
        stop("'recursive' must be an integer of length 1 or a logical")
    }
    L <- .Call("_h5dump", loc$H5Identifier@ID, depth, index_type, order, PACKAGE='rhdf5')
    if (load) {
        L <- h5loadData( loc$H5Identifier, L, all=all, ..., native=native)
    } else {
        L <- h5lsConvertToDataframe(L, all=all, native=native)
    }
    
    L
}

