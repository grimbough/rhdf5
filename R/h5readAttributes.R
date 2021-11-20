#' Read all attributes from a given location in an HDF5 file
#'
#' @param file Character vector of length 1, giving the path to the HDF5
#' @param name Path within the HDF5 file to the object whose attributes should
#'   be read.
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#'   are treated as stored in HDF5 row-major rather than R column-major
#'   orientation.
#' @param \dots Further arguments passed to \code{\link{H5Aread}}.
#'
#' @returns A named list of the same length as the number of attributes attached
#'   to the specific object.  The names of the list entries correspond to the
#'   attribute names.  If no attributes are found an empty list is returned.
#'
#' @name h5_readAttributes
#' @export
h5readAttributes <- function(file, name, native = FALSE, ...) {
  loc = h5checktypeOrOpenLoc(file, readonly=TRUE, native = native)
  on.exit(h5closeitLoc(loc))
  if (!H5Lexists(loc$H5Identifier, name)) {
    stop("Object ", name, " does not exist in this HDF5 file.")
  } else {
    oid = H5Oopen(loc$H5Identifier, name)
    on.exit(H5Oclose(oid), add = TRUE)
    type = H5Iget_type(oid)
    num_attrs = H5Oget_num_attrs(oid)
    if (is.na(num_attrs)) { num_attrs = 0 }
    res = list()
    if (num_attrs > 0) {
      for (i in seq_len(num_attrs)) {
        A = H5Aopen_by_idx(loc$H5Identifier, n = i-1, objname = name)
        attrname <- H5Aget_name(A)
        res[[attrname]] = H5Aread(A, ...)
        H5Aclose(A)
      }
    }
  } 
  res
}
