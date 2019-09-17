h5exists <- function(file, name) {
    loc = h5checktypeOrOpenLoc(file, readonly=TRUE, native = native)
    on.exit( h5closeitLoc(loc) )
    H5Lexists(loc, name = name)
}