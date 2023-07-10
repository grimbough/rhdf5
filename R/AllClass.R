#' An S4 class representing an H5 object
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


#' An S4 class representing H5 references.
#'
#' A class representing one or more HDF5 references.
#'
#' The length of the `val` slot is dependent on both the number and type of
#' references stored in the object. `H5R_OBJECT` references are stored in 8
#' bytes, while `H5R_DATASET_REGION` references require 12 bytes.  The length
#' of `val` will then be a multiple of 8 or 12 respectively.  This also means 
#' that references of different types cannot be combined in a single object.
#'
#' @slot val `raw` vector containing the byte-level representation of each
#'   reference.
#' @slot type `integer` of length 1, which maps to either `H5R_OBJECT` or
#'   `H5R_DATASET_REGION`.
#'
#' @param object Object of class `H5Ref`
setClass("H5Ref",
         slots = c(val = "raw",
                   type = "integer")
)

#' #' @importClassesFrom S4Vectors Factor
#' setClass("H5Enum",
#'          contains = "Factor"
#' )