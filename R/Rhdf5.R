#' List all open HDF5 objects.
#'
#' A list of all valid HDF5 identifiers. H5 objects should be closed after usage
#' to release resources.
#'
#' @param native An object of class `logical`. If TRUE, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using `native = TRUE` increases HDF5 file portability
#' between programming languages. A file written with `native = TRUE`
#' should also be read with `native = TRUE`
#' @returns `h5validObjects` returns a list of [H5IdComponent-class]
#' objects. `h5listIdentifier` prints the valid identifiers on screen and
#' returns NULL.
#' @author Bernd Fischer, Mike Smith
#' @examples
#'
#' h5File <- tempfile("ex_list_identifier.h5")
#'
#' h5createFile(h5File)
#'
#' # create groups
#' h5createGroup(h5File, "foo")
#'
#' h5listIdentifier()
#' h5validObjects()
#'
#' @name h5listObjects
NULL

#' @rdname h5listObjects
#' @export
h5listIdentifier <- function() {
  res <- .Call("_h5listIdentifier", PACKAGE = "rhdf5")
  res$type <- h5const2String("H5I_TYPE", res$type)
  res <- as.data.frame(res)
  res
}

#' @rdname h5listObjects
#' @export
h5validObjects <- function(native = FALSE) {
  ids <- .Call("_h5validObjects", PACKAGE = "rhdf5")
  res <- list()
  for (i in seq_along(ids)) {
    res[[i]] <- new("H5IdComponent", ID = ids[i], native = native)
  }
  res
}

h5listOpenObjects <- function(h5file) {
  h5checktype(h5file, "file")
  invisible(.Call("_h5listOpenObjects", h5file@ID, PACKAGE = "rhdf5"))
}

getDatatypeName <- function(type) {
  .Call("_getDatatypeName", type, PACKAGE = "rhdf5")
}

getDatatypeClass <- function(type) {
  .Call("_getDatatypeClass", type, PACKAGE = "rhdf5")
}

#' Print the rhdf5 and libhdf5 version numbers
#'
#' Returns the version number of the Bioconductor package rhdf5 and the
#' C-library libhdf5.
#'
#'
#' @returns A list of major, minor and release number.
#' @author Bernd Fischer, Mike L. Smith
#' @examples
#'
#' h5version()
#'
#' @importFrom utils packageVersion
#' @export
h5version <- function() {
  part1 <- sprintf(
    "This is Bioconductor rhdf5 %s linking to C-library HDF5 %s (Rhdf5lib version: %s)",
    as.character(packageVersion("rhdf5")),
    paste(H5get_libversion(), collapse = "."),
    as.character(packageVersion("Rhdf5lib"))
  )

  part2 <- ifelse(
    requireNamespace("rhdf5filters"),
    paste0(" and rhdf5filters ", as.character(packageVersion("rhdf5filters"))),
    ""
  )
  message(part1, part2)
  invisible(NULL)
}
