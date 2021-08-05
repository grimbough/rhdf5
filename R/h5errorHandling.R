#' Set how HDF5 error messages are displayed
#' 
#' Sets the options for handling HDF5 error messages in the R sessions.
#' 
#' @param type 'normal' (default) shows a one line error message in R.
#' 'verbose' shows the whole HDF5 error message. 'suppress' suppresses the HDF5
#' error messages completely.
#' @return Returns 0 if options are set successfully.
#' @author Bernd Fischer
#' @seealso \link{rhdf5}
#' @examples
#' 
#' h5errorHandling("normal")
#' 
#' @name h5_errorHandling
#' @export h5errorHandling
h5errorHandling <- function(type = "normal") {

  t = switch(type,
         "suppress" = 0L,
         "verbose" = 2L,
         1L)
  .Call("_h5errorHandling", t, PACKAGE = "rhdf5")

  invisible(NULL)
}

