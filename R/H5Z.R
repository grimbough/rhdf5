
#' Determine whether a filter is available on this system
#' 
#' @param filter_id Integer representing the ID of the filter to be checked.
#' 
#' @export
H5Zfilter_avail <- function(filter_id) {
    filter_id <- as.integer(filter_id)
    res <- .Call("_H5Zfilter_avail", filter_id, PACKAGE = "rhdf5")
    return( res );
}