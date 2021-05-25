
#' Return the dimensions of a dataset chunk
#' 
#' @details This function does not map directly to the HDF5 C API but is 
#' included as a useful addition.
#' 
#' @param h5dataset Object of class [H5IdComponent] representing an open HDF5 
#' dataset.
#' 
#' @return If the supplied dataset is chunked returns a vector, with length
#' equal to the rank of the dataset, containing the size of the dataset 
#' dimensions.  Returns `NULL` if the given dataset is not chunked.
#' 
#' @author Mike Smith
#' 
#' @export
H5Dchunk_dims <- function(h5dataset) {
    h5checktype(h5dataset, "dataset")
    
    pid <- H5Dget_create_plist(h5dataset)
    on.exit(H5Pclose(pid), add=TRUE)
    
    if (H5Pget_layout(pid) != "H5D_CHUNKED")
        return(NULL)
    else 
        return(rev(H5Pget_chunk(pid)))
}
