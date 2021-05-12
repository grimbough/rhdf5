
#' Create an HDF5 file
#' 
#' @param name The name of the HDF5 file to create.
#' @param flags See \code{h5const("H5F_ACC")} for possible arguments.
#' @param fcpl,fapl Object object of class [H5IdComponent-class].  This should 
#' representing a file creation property list and a file access property list
#' respectively. See [H5Pcreate()] or  [H5Pcopy()] to create objects of this 
#' kind.  Leaving as `NULL` will use the default HDF5 settings which are often
#' sufficient.
#' @param native An object of class \code{logical}. If `TRUE`, array-like 
#' objects are treated as stored in HDF5 row-major rather than R column-major 
#' orientation. Using \code{native = TRUE} increases HDF5 file portability 
#' between programming languages. A file written with \code{native = TRUE} 
#' should also be read with \code{native = TRUE}.
#' 
#' @export
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

#' @export
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

#' @export
H5Fclose <- function( h5file ) {
  h5checktype(h5file, "file")
  invisible(.Call("_H5Fclose", h5file@ID, PACKAGE='rhdf5'))
}

#' @export
H5Fflush <- function( h5file, scope = h5default("H5F_SCOPE") ) {
  h5checktype(h5file, "file")
  scope <- h5checkConstants( "H5F_SCOPE", scope )
  invisible(.Call("_H5Fflush", h5file@ID, scope, PACKAGE='rhdf5'))
}

#' Determine whether a file is in the HDF5 format
#' 
#' `H5Fis_hdf5()` determines whether a file is in the HDF5 format.
#' 
#' @param name Character vector of length 1, giving the path to the file to
#' be checked.
#' @param showWarnings If the file doesn't exist an warning is generated.
#' Setting this argument to `FALSE` will suppress the warning.
#' 
#' @return Returns `TRUE`, if the file is an HDF5 file, or `FALSE` otherwise. 
#' In the case the file doesn't exist, `NA` is returned
#' 
#' @export
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

#' @export
H5Fget_filesize <- function( h5file ) {
  h5checktype(h5file, "file")
  .Call("_H5Fget_filesize", h5file@ID, PACKAGE = 'rhdf5')
}

#' Get property lists associated with an HDF5 file
#' 
#' @param h5file An object of class [H5IdComponent-class] representing a H5
#' file identifier.  Typically produced by [H5Fopen()] or [H5Fcreate()].
#' 
#' @name H5Fget_plist


#' @rdname H5Fget_plist
#' @export
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

#' @rdname H5Fget_plist
#' @export
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

#' Retrieve the name of the file to which an object belongs
#' 
#' @param h5obj An object of class [H5IdComponent-class].  Despite this being
#' an H5F function, it works equally well on H5 file, group, dataset and 
#' attribute datatypes.
#' 
#' @examples 
#' 
#' ## use an example file and show its location
#' h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
#' h5file
#' 
#' ## open a file handle and confirm we can identify the file it points to
#' fid <- H5Fopen(h5file)
#' H5Fget_name(fid)
#' 
#' ## H5Fget_name() can be applied to group and dataset handles too
#' gid <- H5Gopen(fid, name = "/")
#' did <- H5Dopen(fid, name = "DS1")
#' H5Fget_name(gid)
#' H5Fget_name(did)
#' 
#' ## tidy up
#' H5Dclose(did)
#' H5Gclose(gid)
#' H5Fclose(fid)
#' 
#' @export
H5Fget_name <- function( h5obj ) {
  h5checktype(h5obj, "object")
  .Call("_H5Fget_name", h5obj@ID, PACKAGE='rhdf5')
}
