
h5set_extent <- function(file, dataset, dims, native = FALSE) {

    loc = h5checktypeOrOpenLoc(file, native = native)
    on.exit( h5closeitLoc(loc) )
    
    if (is.character(dataset)) {
        if (!H5Lexists(loc$H5Identifier, dataset)) {
            stop("Object ",dataset," does not exist in this HDF5 file.")
        } else {
            did = H5Oopen(loc$H5Identifier, dataset)
            type = H5Iget_type(did)
            if (type != "H5I_DATASET") {
                H5Oclose(did)
                stop("'", dataset, "' is not a dataset.")
            }
            res = H5Dset_extent(did, dims)
            H5Oclose(did)
        }
    } else {
        h5checktype(dataset, "dataset")
        ## TODO: only valid for chunked datasets, so we should check for them
        res = H5Dset_extent(dataset, dims)
    }
    
    invisible(res)
}
