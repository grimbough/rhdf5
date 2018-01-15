

h5delete <- function(file, name) {
    
    loc <- h5checktypeOrOpenLoc(file, native=FALSE)
    on.exit( h5closeitLoc(loc) )
    
    H5Ldelete( h5loc = loc$H5Identifier, name = name )
    
}
