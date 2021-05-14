#' Set a new dataset extension
#' 
#' Set a new dataset extension to an existing dataset in an HDF5 file
#' #' 
#' @param file The filename (character) of the file in which the dataset will
#' be located. For advanced programmers it is possible to provide an object of
#' class \code{\link{H5IdComponent}} representing a H5 location identifier
#' (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}},
#' \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this
#' kind.
#' @param dataset The name of the dataset in the HDF5 file, or an object of
#' class \code{\link{H5IdComponent}} representing a H5 dataset identifier. See
#' \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of
#' this kind.
#' @param dims The dimensions of the array as they will appear in the file.
#' Note, the dimensions will appear in inverted order when viewing the file
#' with a C program (e.g. HDFView), because the fastest changing dimension in
#' R is the first one, whereas the fastest changing dimension in C is the last
#' one.
#' @param native An object of class \code{logical}. If `TRUE`, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using \code{native = TRUE} increases HDF5 file portability
#' between programming languages. A file written with \code{native = TRUE}
#' should also be read with \code{native = TRUE}
#' @return Returns 0 if the dimension of the dataset was changed successfully
#' and a negative value otherwise.
#' @author Bernd Fischer
#' @examples
#' 
#' tmpfile <- tempfile()
#' h5createFile(file=tmpfile)
#' h5createDataset(tmpfile, "A", c(10,12), c(20,24))
#' h5ls(tmpfile, all=TRUE)[c("dim", "maxdim")]
#' h5set_extent(tmpfile, "A", c(20,24))
#' h5ls(tmpfile, all=TRUE)[c("dim", "maxdim")]
#' 
#' @name h5_set_extent
#' @export h5set_extent
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
