
H5Fcreate <- function( name, flags = h5default("H5F_ACC"), fcpl = NULL, fapl = NULL, native = FALSE) {

    if (length(name)!=1 || !is.character(name)) 
        stop("'name' must be a character string of length 1")
    name <- normalizePath(name, mustWork = FALSE)
    if(.Platform$OS.type == "windows") {
      name <- enc2utf8(name)
    }
    
    flags <- h5checkConstants( "H5F_ACC", flags )
    fcpl = h5checktypeAndPLC(fcpl, "H5P_FILE_CREATE", allowNULL = TRUE)
    if (is.null(fapl)) {
        ## create a new property list, and make sure it is closed
        fapl = H5Pcreate("H5P_FILE_ACCESS")
        on.exit(H5Pclose(fapl))
        H5Pset_libver_bounds(fapl, libver_low = "H5F_LIBVER_EARLIEST", libver_high = "H5F_LIBVER_LATEST")
    }
    fapl = h5checktypeAndPLC(fapl, "H5P_FILE_ACCESS", allowNULL = TRUE)
    fid <- .Call("_H5Fcreate", name, flags, fcpl@ID, fapl@ID, PACKAGE='rhdf5')
    if (fid > 0) {
        h5file = new("H5IdComponent", ID = fid, native = native)
    } else {
        message("HDF5: unable to create file")
        h5file = FALSE
    }
    invisible(h5file)
}

H5Fopen <- function( name, flags = h5default("H5F_ACC_RD"), fapl = NULL, native = FALSE ) {
  
  if (length(name)!=1 || !is.character(name)) {
    stop("'name' must be a character string of length 1")
  }
  if(!grepl("^http[s]?://", x = name)) {
    name <- normalizePath(name, mustWork = FALSE)
  }
  flags <- h5checkConstants( "H5F_ACC_RD", flags )
  
  if (is.null(fapl)) {
    ## create a new file access property list
    fapl <- H5Pcreate("H5P_FILE_ACCESS")
    on.exit(H5Pclose(fapl))
  } else {
    fapl <- h5checktypeAndPLC(fapl, "H5P_FILE_ACCESS", allowNULL = FALSE)
  }
  
  fid <- .Call("_H5Fopen", name, flags, fapl@ID, PACKAGE='rhdf5')
  if (fid > 0) {
    h5file = new("H5IdComponent", ID = fid, native = native)
  } else {
    message("HDF5: unable to open file")
    h5file = FALSE
  }
  invisible(h5file)
}

## H5Freopen <- function( h5file ) {
##   ## h5checktype(h5file, "file")
##   fid <- .Call("_H5Freopen", h5file@ID, PACKAGE='rhdf5')
##   if (fid > 0) {
##     h5file = new("H5file", ID = fid)
##   } else {
##     message("HDF5: unable to open file")
##     h5file = FALSE
##   }
##   invisible(h5file)
## }

H5Fclose <- function( h5file ) {
  h5checktype(h5file, "file")
  invisible(.Call("_H5Fclose", h5file@ID, PACKAGE='rhdf5'))
}

H5Fflush <- function( h5file, scope = h5default("H5F_SCOPE") ) {
  h5checktype(h5file, "file")
  scope <- h5checkConstants( "H5F_SCOPE", scope )
  invisible(.Call("_H5Fflush", h5file@ID, scope, PACKAGE='rhdf5'))
}

H5Fis_hdf5 <- function( name, showWarnings=TRUE ) {

    if (length(name)!=1 || !is.character(name)) 
        stop("'name' must be a character string of length 1")
    name <- normalizePath(name, mustWork = FALSE)
    res <- NA
    if (file.exists(name)) {
        res = .Call("_H5Fis_hdf5", name, PACKAGE = 'rhdf5')
    } else {
        if (showWarnings) {
            warning("File does not exist.")
        }
    }
    res
}

H5Fget_filesize <- function( h5file ) {
  h5checktype(h5file, "file")
  .Call("_H5Fget_filesize", h5file@ID, PACKAGE = 'rhdf5')
}

H5Fget_create_plist <- function( h5file ) {
  h5checktype(h5file, "file")
  pid <- .Call("_H5Fget_create_plist", h5file@ID, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plist = new("H5IdComponent", ID = pid, native = h5file@native)
  } else {
    message("HDF5: unable to create property list")
    h5plist = FALSE
  }
  invisible(h5plist)
}


H5Fget_access_plist <- function( h5file ) {
  h5checktype(h5file, "file")
  pid <- .Call("_H5Fget_access_plist", h5file@ID, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plist = new("H5IdComponent", ID = pid, native = h5file@native)
  } else {
    message("HDF5: unable to create property list")
    h5plist = FALSE
  }
  invisible(h5plist)
}

H5Fget_name <- function( h5obj ) {
  h5checktype(h5obj, "object")
  .Call("_H5Fget_name", h5obj@ID, PACKAGE='rhdf5')
}
