#' An S4 class representing H5 object
#'
#' A class representing a HDF5 identifier handle. HDF5 identifiers represent
#' open files, groups, datasets, dataspaces, attributes, and datatypes.
#'
#' @slot ID `Integer` of length 1. Contains the handle of C-type `hid_t`.
#' @slot native An object of class \code{logical}. If TRUE, array-like objects
#'   are treated as stored in HDF5 row-major rather than R column-major
#'   orientation. Using \code{native = TRUE} increases HDF5 file portability
#'   between programming languages. A file written with \code{native = TRUE}
#'   should also be read with \code{native = TRUE}
setClass("H5IdComponent",
         representation(ID = "character", native = "logical")
)
