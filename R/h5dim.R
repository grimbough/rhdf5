h5dim <- function(file, name, native = FALSE) {
    
    loc = h5checktypeOrOpenLoc(file, readonly=TRUE, native = native)
    on.exit( h5closeitLoc(loc) )
    
    if (!H5Lexists(loc$H5Identifier, name)) {
        stop("Object '", name, "' does not exist in this HDF5 file.")
    } else {
        
        oid = H5Oopen(loc$H5Identifier, name)
        type = H5Iget_type(oid)
        on.exit(H5Oclose(oid), add = TRUE)
        
        if (type == "H5I_DATASET") {
            
            did <- H5Dopen(loc$H5Identifier, name)
            on.exit(H5Dclose(did), add = TRUE)
            sid <- H5Dget_space(did)
            on.exit(H5Sclose(sid), add = TRUE)
            dim <- H5Sget_simple_extent_dims(sid)$size
            
        } else {
            stop("Dimensions can only be returned for datasets.")
        }
        
    }
    return(dim)
}

h5length <- function(file, name) {
    
    dim <- h5dim(file, name, native = FALSE)
    return( prod(dim) )
}

h5chunkdim <- function(file, name, native = FALSE) {
    
    loc = h5checktypeOrOpenLoc(file, readonly=TRUE, native = native)
    on.exit( h5closeitLoc(loc) )
    
    if (!H5Lexists(loc$H5Identifier, name)) {
        stop("Object '", name, "' does not exist in this HDF5 file.")
    } else {
        
        oid = H5Oopen(loc$H5Identifier, name)
        type = H5Iget_type(oid)
        on.exit(H5Oclose(oid), add = TRUE)
        
        if (type == "H5I_DATASET") {
            
            did <- H5Dopen(loc$H5Identifier, name)
            on.exit(H5Dclose(did), add = TRUE)
            pid <- H5Dget_create_plist(did)
            on.exit(H5Pclose(pid), add = TRUE)
            if (H5Pget_layout(pid) != "H5D_CHUNKED")
                return(NULL)
            else {
                chunkdim <- H5Pget_chunk(pid)
                if(!native) {
                    chunkdim <- rev(chunkdim)
                }
            }
            
        } else {
            stop("Chunk dimensions can only be returned for datasets.")
        }
        
    }
    return(chunkdim)
    
}