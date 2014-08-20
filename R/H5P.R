
H5Pcreate <- function( type = h5default("H5P") ) {
  type <- h5checkConstants( "H5P", type )
  pid <- .Call("_H5Pcreate", type, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plist = new("H5IdComponent", ID = pid)
  } else {
    message("HDF5: unable to create property list")
    h5plist = FALSE
  }
  invisible(h5plist)
}

H5Pget_class <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  pclid <- .Call("_H5Pget_class", h5plist@ID, PACKAGE='rhdf5')
  if (pclid > 0) {
    h5plistclass = new("H5IdComponent", ID = pclid)
  } else {
    message("HDF5: unable to get property list class")
    h5plistclass = FALSE
  }
  invisible(h5plistclass)
}

H5Pcopy <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  pid <- .Call("_H5Pcopy", h5plist@ID, PACKAGE='rhdf5')
  if (pid > 0) {
    h5plistnew = new("H5IdComponent", ID = pid)
  } else {
    message("HDF5: unable to copy property list")
    h5plistnew = FALSE
  }
  invisible(h5plistnew)
}

H5Pclose <- function( h5plist ) {
  h5checktype(h5plist, "plist")
  invisible(.Call("_H5Pclose", h5plist@ID, PACKAGE='rhdf5'))
}

H5Pclose_class <- function( h5plistclass ) {
  h5checktype(h5plistclass, "plistclass")
  invisible(.Call("_H5Pclose_class", h5plistclass@ID, PACKAGE='rhdf5'))
}



