#' Read the time stamps associated with an HDF5 group or dataset.
#'
#' @param file Character vector of length 1, giving the path to the HDF5 file
#' @param name Path within the HDF5 file to the object whose attributes should
#'   be read. The datasets present in \code{file} can be listed with the
#'  function \code{\link{h5ls}}.
#'
#' @returns A named list of length 4 containing the timestamps on the object.
#' The timestamps themselves are `POSIXct` objects (see
#' [base::DateTimeClasses()]).
#'
#' @details All timestamps are returned in the UTC timezone.  HDF5 objects
#' can have between 0 and 4 timestamps set, depending on the property lists
#' provided when they are created or accessed.  Timestamps that are not tracked
#' will be returned as the UNIX epoch `1970-01-01 UTC`.
#'
#' @examples
#' # example file
#' example_file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
#'
#' ## read timestamps on a group
#' h5readTimestamps(example_file, name = "/")
#'
#' ## read timestamps on a datasets
#' h5readTimestamps(example_file, name = "/DS1")
#'
#' @name h5readTimestamps
#' @export
h5readTimestamps <- function(file, name) {
  loc <- h5checktypeOrOpenLoc(file, readonly = TRUE, native = FALSE)
  on.exit(h5closeitLoc(loc))
  if (!H5Lexists(loc$H5Identifier, name)) {
    stop(
      "Object ", name, " does not exist in this HDF5 file.\n",
      "Use `h5ls()` to list the objects in the file."
    )
  }
  oid <- H5Oopen(loc$H5Identifier, name)
  on.exit(H5Oclose(oid), add = TRUE)
  info <- H5Oget_info(oid)
  res <- info[4:7]
  return(res)
}
