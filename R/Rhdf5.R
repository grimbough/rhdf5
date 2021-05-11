#' list all valid H5 identifier.
#' 
#' A list of all valid H5 identifier. H5 objects should be closed after usage
#' to release resources.
#' 
#' 
#' @aliases h5listIdentifier h5validObjects
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using \code{native = TRUE} increases HDF5 file portability
#' between programming languages. A file written with \code{native = TRUE}
#' should also be read with \code{native = TRUE}
#' @return \code{h5validObjects} returns a list of \code{\link{H5IdComponent}}
#' objects. \code{h5listIdentifier} prints the valid identifiers on screen and
#' returns NULL.
#' @author Bernd Fischer
#' @seealso \link{rhdf5}
#' @references \url{https://portal.hdfgroup.org/display/HDF5}
#' @keywords programming interface IO file
#' @examples
#' 
#' h5createFile("ex_list_identifier.h5")
#' 
#' # create groups
#' h5createGroup("ex_list_identifier.h5","foo")
#' 
#' h5listIdentifier()
#' h5validObjects()
#' 
#' @export h5listIdentifier
h5listIdentifier <- function() {
  res <- .Call("_h5listIdentifier", PACKAGE='rhdf5')
  res$type = h5const2String("H5I_TYPE",res$type)
  res = as.data.frame(res)
  res
}

h5validObjects <- function(native = FALSE) {
  ids <- .Call("_h5validObjects", PACKAGE='rhdf5')
  res <- list()
  for (i in seq_len(length(ids))) {
    res[[i]] <- new("H5IdComponent", ID = ids[i], native = native)
  }
  res
}

h5listOpenObjects <- function(h5file) {
  h5checktype(h5file, "file")
  invisible(.Call("_h5listOpenObjects", h5file@ID, PACKAGE = "rhdf5"))
}

getDatatypeName <- function(type) {
  .Call("_getDatatypeName", type, PACKAGE='rhdf5')
}

getDatatypeClass <- function(type) {
  .Call("_getDatatypeClass", type, PACKAGE='rhdf5')
}







#' Print the rhdf5 and libhdf5 version numbers
#' 
#' Returns the version number of the Bioconductor package rhdf5 and the
#' C-library libhdf5.
#' 
#' 
#' @return A list of major, minor and release number.
#' @author Bernd Fischer, Mike L. Smith
#' @examples
#' 
#' h5version()
#' 
#' @export h5version
h5version <- function() {

  part1 <- paste0("This is Bioconductor rhdf5 ",
                  as.character(packageVersion("rhdf5")),
                  " linking to C-library HDF5 ",
                  paste(H5get_libversion(), collapse="."))
  
  part2 <- ifelse(requireNamespace('rhdf5filters'),
                  paste0(" and rhdf5filters ", 
                         as.character(packageVersion('rhdf5filters'))), 
                  "")
  message(part1, part2)
  invisible(NULL)
}
