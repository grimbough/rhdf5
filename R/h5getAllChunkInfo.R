#' Iterate over chunks of an HDF5 dataset
#'
#' Returns the offset, filter mask, file address, and byte size of every
#' stored chunk in a chunked HDF5 dataset.  This is a thin wrapper around
#' the HDF5 C function \code{h5getAllChunkInfo} and requires that the dataset
#' uses chunked storage; contiguous or compact datasets will raise an error.
#'
#' @param h5dataset An object of class \linkS4class{H5IdComponent} representing
#'   an open HDF5 dataset, as returned by \code{\link{H5Dopen}}.
#'
#' @return A named list with four elements, one entry per stored chunk:
#' \describe{
#'   \item{offset}{Integer matrix with one row per chunk and one column per
#'     dataset dimension, giving the chunk's logical origin in element
#'     coordinates.}
#'   \item{filter_mask}{Integer vector of per-chunk filter pipeline bitmasks.
#'     A value of \code{0} means all filters were applied.}
#'   \item{addr}{Numeric vector of byte offsets within the HDF5 file at which
#'     each chunk's data begins.}
#'   \item{size}{Integer vector of compressed (on-disk) byte sizes for each
#'     chunk.}
#' }
#'
#' @examples
#' ## chunked dataset: szip-compressed 2-D integer array
#' h5file <- system.file("testfiles", "h5ex_d_szip.h5", package = "rhdf5")
#' fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
#' did <- H5Dopen(fid, "DS1")
#'
#' ci <- h5getAllChunkInfo(did)
#' head(ci$offset)   # logical chunk origins (element coordinates)
#' ci$addr[1]        # file offset of first chunk in bytes
#' ci$size[1]        # compressed size of first chunk in bytes
#'
#' H5Dclose(did)
#' H5Fclose(fid)
#'
#' @seealso \code{\link{H5Dopen}}, \code{\link{H5Dget_num_chunks}}
#' @export
h5getAllChunkInfo <- function(h5dataset) {
  h5checktype(h5dataset, "dataset")
  .Call("_h5getAllChunkInfo", h5dataset@ID, PACKAGE = "rhdf5")
}
