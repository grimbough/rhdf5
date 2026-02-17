#' Deprecated
#'
#' Deprecated in line with changes in the upstream HDF5 library.
#' It will be removed in the next version of rhdf5.
#'
#' @returns NULL (invisibly)
#'
#' @export
H5Pget_version <- function(...) {
  # A quick check across Bioconductor suggests nobody was using this function
  .Deprecated(
    msg = paste0(
      "H5Pget_version() has been deprecated in line with changes in the upstream HDF5 library. ",
      "It will be removed in the next version of rhdf5."
    )
  )
  invisible(NULL)
}
