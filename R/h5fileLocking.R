#' Test and set file locking for HDF5
#' 
#' HDF5 1.10 uses file locking by default.  On some file systems this is not
#' available, and the HDF5 library will throw an error if the user attempts to
#' create or access a file located on such a file system.  These functions help
#' identify if file locking is available without throwing an error, and allow
#' the locking to be disabled for the duration of the R session if needed.
#' 
#' \code{h5testFileLocking} will create a temporary file and then attempt to
#' apply a file lock using the appropriate function within the HDF5 library.
#' The success or failure of the locking is then recorded and the temporary
#' file removed.  Even relatively low level functions such as
#' \code{\link{H5Fcreate}} will fail inelegantly if file locking fails.
#' 
#' \code{h5disableFileLocking} will set the environment variable
#' \code{RHDF5_USE_FILE_LOCKING=FALSE}, which is the recommended was to disable
#' this behaviour if file locking is not supported.  This will only persist
#' within the current R session.  You can set the environment variable outside
#' of R if this is a more general issue on your system.
#' 
#' \code{h5enableFileLocking} will unset the \code{RHDF5_USE_FILE_LOCKING}.
#' 
#' More discussion of HDF5's use of file locking can be found online e.g.
#' https://forum.hdfgroup.org/t/hdf5-1-10-0-and-flock/3761/4 or
#' https://forum.hdfgroup.org/t/hdf5-files-on-nfs/3985/5
#' 
#' @aliases h5testFileLocking h5enableFileLocking h5disableFileLocking
#' @param location The name of a directory or file to test.  If an existing
#' directory is provided a temporary file will be created in this folder.  If
#' non-existant location is provided a file with the name will be created,
#' tested for file locking, and then removed.  Providing an existing file will
#' result in an error.
#' @return \code{h5testFileLocking} returns \code{TRUE} if a file can be
#' successfully locked at the specified location, or \code{FALSE} otherwise.
#' 
#' \code{h5disableFileLocking} and \code{h5enableFileLocking} set are called
#' for the side effect of setting or unsetting the environment variable
#' \code{HDF5_USE_FILE_LOCKING} and do not return anything.
#' @author Mike Smith
#' @keywords IO file
#' @examples
#' 
#' 
#' ## either a file name or directory can be tested
#' file <- tempfile()
#' dir <- tempdir()
#' 
#' h5testFileLocking(dir)
#' h5testFileLocking(file)
#' 
#' ## we can check for file locking, and disable if needed
#' if( !h5testFileLocking(dir) ) {
#'   h5disableFileLocking()
#' }
#' 
#' @name h5_testFileLocking
#' @importFrom utils file_test
#' @export h5testFileLocking
h5testFileLocking <- function(location) {
  
  if(missing(location)) {
    stop("You must provide a location to test.")
  }
  
  ## stop if existing file passed
  if(file_test("-f", location)) {
    stop('Testing file locking will remove ', location, 
         '\nPlease provide a directory or the name of a temporary file to be created.')
  }
  
  ## passed an existing directory
  if(file_test("-d", location)) {
    file <- tempfile(tmpdir = location)
  } else { ## location doesn't exist, we will create it
    file <- location
  }
  
  lock_status <- .Call("_h5fileLock", file, PACKAGE = "rhdf5")
  
  file.remove(file)
  
  return(lock_status >= 0)
}

h5disableFileLocking <- function() {
  Sys.setenv(HDF5_USE_FILE_LOCKING = "FALSE")
}

h5enableFileLocking <- function() {
  Sys.unsetenv("HDF5_USE_FILE_LOCKING")
}
