#' Write an R object as an HDF5 attribute
#'
#' @param attr The R object to be written as an HDF5 attribute.
#' @param h5obj Normally an object of class [H5IdComponent-class] representing a
#'   H5 object identifier (file, group, or dataset). See
#'   \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}},
#'   \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to
#'   create an object of this kind.  This argument can also be given the path to
#'   an HDF5 file.
#' @param name The name of the attribute to be written.
#' @param h5loc The location of the group or dataset within a file to which the
#'   attribute should be attached. This argument is only used if the
#'   \code{h5obj} argument is the path to an HDF5 file, otherwise it is ignored.
#' @param encoding The encoding of the string data type. Valid options are
#'   "ASCII" and "UTF-8".
#' @param variableLengthString Whether character vectors should be written as
#'   variable-length strings into the attributes.
#' @param asScalar Whether length-1 \code{attr} should be written into a scalar
#'   dataspace.
#' @param checkForNA Whether a \code{attr} should be checked for \code{NA}
#'   values before being written.  This only applies of \code{attr} is of type
#'   logical.  Testing for \code{NA} values can be slow if the object to be
#'   written is large, so if you are sure no such values will be present this
#'   argument can be used to disable the testing.
#' @name h5_writeAttribute
#' @export
h5writeAttribute <- function(attr, h5obj, name, h5loc, encoding = NULL, 
                             variableLengthString=FALSE, asScalar=FALSE,
                             checkForNA = TRUE) {
  
  if (is(attr, "H5IdComponent"))
    res <- h5writeAttribute.array(attr, h5obj, name, asScalar=TRUE)
  else
    res <- UseMethod("h5writeAttribute")
  invisible(res)
}

#' @export
h5writeAttribute.matrix <- function(...) { h5writeAttribute.array(...) }
#' @export
h5writeAttribute.integer <- function(...) { h5writeAttribute.array(...) }
#' @export
h5writeAttribute.double <- function(...) { h5writeAttribute.array(...) }
#' @export
h5writeAttribute.logical <- function(...) { h5writeAttribute.array(...) }
#' @export
h5writeAttribute.character <- function(...) { h5writeAttribute.array(...) }
#' @export
h5writeAttribute.default <- function(attr, h5obj, name, ...) { warning("No function found to write attribute of class '",class(attr),"'. Attribute '",name,"' is not written to hdf5-file.") }

#' @rdname h5_writeAttribute
h5writeAttribute.array <- function(attr, h5obj, name, h5loc, encoding = NULL, 
                                   variableLengthString=FALSE, asScalar=FALSE,
                                   checkForNA = TRUE) {
  
  if(is.character(h5obj) && file.exists(h5obj)) {
    fid <- H5Fopen(h5obj, flags = "H5F_ACC_RDWR")
    on.exit(H5Fclose(fid))
    h5obj <- H5Oopen(h5loc = fid, name = h5loc)
    on.exit(H5Oclose(h5obj), add = TRUE)
  } else {
    h5checktype(h5obj, "object")
  }

  if (asScalar) {
    if (length(attr) != 1L) {
      stop("cannot use 'asScalar=TRUE' when 'length(attr) > 1'")
    }
    dims <- NULL
  } else {
    dims <- dim(attr)
    if (is.null(dims)) {
      dims <- length(attr)
    }
  }

  size <- NULL
  if (storage.mode(attr) == "character" && !variableLengthString) {
    size <- max(nchar(attr, type = "bytes"))+1
  }

  if (H5Aexists(h5obj, name)) {
    H5Adelete(h5obj, name)
  }
  storagemode <- storage.mode(attr)
  tid <- NULL
  if (storagemode == "S4" && is(attr, "H5IdComponent")) {
    storagemode <- "H5IdComponent"
  } else if (storagemode == "logical") {
    ## should check for NA values if required
    any_na <- ifelse(checkForNA, yes = any(is.na(attr)), no = FALSE)
    
    tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR")
    H5Tenum_insert(tid, name = "TRUE", value = 1L)
    H5Tenum_insert(tid, name = "FALSE", value = 0L)
    if(any_na) 
      H5Tenum_insert(tid, name = "NA", value = 255L)
  }

  h5createAttribute(h5obj, name, dims = dims, storage.mode = storagemode, 
                    size = size, H5type = tid,
                    encoding = match.arg(encoding, choices = c("ASCII", "UTF-8", "UTF8")))
  h5attr <- H5Aopen(h5obj, name)

  DimMem <- dim(attr)
  h5spaceMem <- H5Screate_simple(DimMem,NULL)

  res <- H5Awrite(h5attr, attr) 

  H5Sclose(h5spaceMem) 
  H5Aclose(h5attr) 
  invisible(res)
}

