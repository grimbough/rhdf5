
#' Additional functions for finding details of dataset chunking.
#' 
#' @details These functions do not map directly to the HDF5 C API but follow the same style and are 
#' included as potentially useful additions.
#' 
#' * `H5Dis_chunked` tests whether a dataset is chunked.
#' * `H5Dchunk_dims` will return the dimensions of the dataset chunks.
#' 
#' @param h5dataset Object of class [H5IdComponent-class] representing an open HDF5 
#' dataset.
#' 
#' @returns
#' * `H5Dchunk_dims`: If the supplied dataset is chunked returns a vector, with length
#' equal to the rank of the dataset, containing the size of the dataset 
#' dimensions.  Returns `NULL` if the given dataset is not chunked.
#' * `H5Dis_chunked`: returns `TRUE` if a dataset is chunked and `FALSE` otherwise.
#' 
#' @author Mike Smith
#' 
#' @name H5D_extras
NULL

#' @rdname H5D_extras
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

#' @rdname H5D_extras
#' @export
H5Dis_chunked <- function(h5dataset) {
  h5checktype(h5dataset, "dataset")
  
  pid <- H5Dget_create_plist(h5dataset)
  on.exit(H5Pclose(pid), add=TRUE)
  
  return(H5Pget_layout(pid) == "H5D_CHUNKED")
}

