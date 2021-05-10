

h5delete <- function(file, name) {
    
    loc <- h5checktypeOrOpenLoc(file, native=FALSE)
    on.exit( h5closeitLoc(loc) )
    
    H5Ldelete( h5loc = loc$H5Identifier, name = name )
    
}


h5deleteAttribute <- function(file, name, attribute) {
    
    loc <- h5checktypeOrOpenLoc(file, native=FALSE)
    on.exit( h5closeitLoc(loc) )
    
    res <- FALSE
    if (H5Lexists(loc$H5Identifier, name)) {
        oid <- H5Oopen(loc$H5Identifier, name = name)
        on.exit(H5Oclose(oid), add = TRUE)
        if(H5Aexists(h5obj = oid, name = attribute)) {
            res <- !(H5Adelete(h5obj = oid, name = attribute))
        } else {
            message("Attribute '", attribute, "' not found.")
        }
    } else {
        message("Object '", name, "' not found in ", file)
    }
    return(invisible(res))
}
