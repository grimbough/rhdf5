#' Write an R object as an HDF5 attribute
#'
#' @param attr The R object to be written as an HDF5 attribute.
#' @param h5obj An object of class [H5IdComponent-class] representing a H5
#'   object identifier (file, group, or dataset). See \code{\link{H5Fcreate}},
#'   \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}},
#'   \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of
#'   this kind.
#' @param name The name of the attribute to be written.
#' @param encoding The encoding of the string data type. Valid options are
#'   "ASCII" and "UTF-8".
#' @param cset *Deprecated in favour of the `encoding` argument.*
#' @param variableLengthString Whether character vectors should be written as
#'   variable-length strings into the attributes.
#' @param asScalar Whether length-1 \code{attr} should be written into a scalar
#'   dataspace.
#'
#' @name h5_writeAttribute
#' @export
h5writeAttribute <- function(attr, h5obj, name, encoding = NULL, cset = NULL,
                             variableLengthString=FALSE, asScalar=FALSE) {
  
  ## remove the cset argument in BioC 3.16
  if(!is.null(cset)) {
    if(is.null(encoding)) 
      encoding <- cset
    message("The 'cset' argument has been deprecated.\n",
            "Please use the argument 'encoding' instead.")
  }
  
  h5checktype(h5obj, "object")
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
h5writeAttribute.array <- function(attr, h5obj, name, encoding = NULL, cset=NULL, 
                                   variableLengthString=FALSE, asScalar=FALSE) {
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
    size <- max(nchar(attr))+1
  }

  if (H5Aexists(h5obj, name)) {
    H5Adelete(h5obj, name)
  }
  storagemode <- storage.mode(attr)
  if (storagemode == "S4" && is(attr, "H5IdComponent"))
    storagemode <- "H5IdComponent"
  h5createAttribute(h5obj, name, dims = dims, storage.mode = storagemode, 
                    size = size, 
                    encoding = match.arg(encoding, choices = c("ASCII", "UTF-8", "UTF8")))
  h5attr <- H5Aopen(h5obj, name)

  DimMem <- dim(attr)
  h5spaceMem <- H5Screate_simple(DimMem,NULL)

  try( { res <- H5Awrite(h5attr, attr) } )

  try( { H5Sclose(h5spaceMem) } )
  try( { H5Aclose(h5attr) } )
  invisible(res)
}

