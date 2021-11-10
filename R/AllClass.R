#' An S4 class representing H5 object
#'
#' A class representing a HDF5 identifier handle. HDF5 identifiers represent
#' open files, groups, datasets, dataspaces, attributes, and datatypes.
#'
#' @slot ID `integer` of length 1. Contains the handle of C-type `hid_t`.
#' @slot native An object of class \code{logical}. If TRUE, array-like objects
#'   are treated as stored in HDF5 row-major rather than R column-major
#'   orientation. Using \code{native = TRUE} increases HDF5 file portability
#'   between programming languages. A file written with \code{native = TRUE}
#'   should also be read with \code{native = TRUE}
#'   
#' @param object Object of class `H5IdComponent`
setClass("H5IdComponent",
         slots = c(ID = "character", 
                   native = "logical")
)


#' An S4 class representing an H5 object reference
#'
#' @slot val `integer` of length 1.
#'   
#' @param object Object of class `H5Ref`
setClass("H5Ref",
         slots = c(val = "raw",
                   type = "integer")
)