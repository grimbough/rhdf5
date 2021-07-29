#' Saves a one or more objects to an HDF5 file.
#' 
#' Saves a number of R objects to an HDF5 file.
#' 
#' The objects will be saved to the HDF5 file. If the file does not exists it
#' will be created. The data can be read again by either [h5dump()]
#' or individually for each dataset by [h5read()].
#' 
#' @param \dots The objects to be saved.
#' @param file The filename (character) of the file in which the dataset will
#' be located. It is also possible to provide an object of
#' class [H5IdComponent-class] representing a H5 location identifier
#' (file or group). See [H5Fcreate()], [H5Fopen()],
#' [H5Gcreate()], [H5Gopen()] to create an object of this
#' kind.
#' @param name A character vector of names for the datasets. The length of the
#' name vector should match the number of objects.
#' @param createnewfile If `TRUE`, a new file will be created if necessary.
#' @param native An object of class \code{logical}. If TRUE, array-like objects
#' are treated as stored in HDF5 row-major rather than R column-major
#' orientation. Using \code{native = TRUE} increases HDF5 file portability
#' between programming languages. A file written with \code{native = TRUE}
#' should also be read with \code{native = TRUE}
#' @return Nothing returned.
#' @author Bernd Fischer
#' @seealso [h5ls()], [h5write()]
#' @examples
#' 
#' A = 1:7;  B = 1:18; D = seq(0,1,by=0.1)
#' h5save(A, B, D, file="ex_save.h5")
#' h5dump("ex_save.h5")
#' 
#' @name h5_save
#' @export h5save
h5save <- function(..., file, name = NULL, createnewfile=TRUE, native = FALSE) {
  N <- length(list(...))

  # get object names
  if (is.null(name)) {
    name <- as.character(substitute(list(...)))[-1L]
  }
  if (length(name) != N) {
    stop("length of 'name' argument  must fit the number of objects to save.")
  }

  loc = h5checktypeOrOpenLoc(file, createnewfile=createnewfile, native = native)

  for (i in 1:N) {
    h5write(list(...)[[i]], file = loc$H5Identifier, name = name[i])
  }

  h5closeitLoc(loc)
  invisible(NULL)
}

