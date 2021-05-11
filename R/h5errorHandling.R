#' handling of HDF5 error messages
#' 
#' Sets the options for handling HDF5 error messages.
#' 
#' Sets the options for HDF5 error hanlding.
#' 
#' @param type 'normal' (default) shows a one line error message in R.
#' 'verbose' shows the whole HDF5 error message. 'suppress' suppresses the HDF5
#' error messages completely.
#' @return Returns 0 if options are set successfully.
#' @author Bernd Fischer
#' @seealso \link{rhdf5}
#' @references \url{https://portal.hdfgroup.org/display/HDF5}
#' @keywords programming interface IO file
#' @examples
#' 
#' h5errorHandling("normal")
#' 
#' @export h5errorHandling
h5errorHandling <- function(type = "normal") {

  t = switch(type,
         "suppress" = 0L,
         "verbose" = 2L,
         1L)
  .Call("_h5errorHandling", t, PACKAGE = "rhdf5")

  invisible(NULL)
}

