#' Delete objects within a HDF5 file
#' 
#' Deletes the specified group or dataset from within an HDF5 file.
#' 
#' @param file The filename (character) of the file in which the object is
#' located.
#' @param name For \code{h5delete} the name of the object to be deleted. For
#' \code{h5deleteAttribute} the name of the object to which the attribute
#' belongs.
#' @author Mike Smith
#' @name h5_delete
#' @export h5delete
h5delete <- function(file, name) {
    
    loc <- h5checktypeOrOpenLoc(file, native=FALSE)
    on.exit( h5closeitLoc(loc) )
    
    H5Ldelete( h5loc = loc$H5Identifier, name = name )
    
}

#' Delete attribute
#' 
#' Deletes an attribute associated with a group or dataset within an HDF5 file.
#' 
#' @param file The filename (character) of the file in which the object is
#' located.
#' @param name The name of the object to which the attribute belongs.
#' @param attribute Name of the attribute to be deleted.
#' 
#' @author Mike Smith
#' @name h5_deleteAttribute
#' @export h5deleteAttribute
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
