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
  Sys.setenv("HDF5_USE_FILE_LOCKING=FALSE")
}

h5enableFileLocking <- function() {
  Sys.unsetenv("HDF5_USE_FILE_LOCKING")
}