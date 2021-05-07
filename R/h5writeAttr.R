
h5writeAttribute <- function(attr, h5obj, name, ...) {
  h5checktype(h5obj, "object")
  res <- UseMethod("h5writeAttribute")
  invisible(res)
}

h5writeAttribute.matrix <- function(...) { h5writeAttribute.array(...) }
h5writeAttribute.integer <- function(...) { h5writeAttribute.array(...) }
h5writeAttribute.double <- function(...) { h5writeAttribute.array(...) }
h5writeAttribute.logical <- function(...) { h5writeAttribute.array(...) }
h5writeAttribute.character <- function(...) { h5writeAttribute.array(...) }
h5writeAttribute.default <- function(attr, h5obj, name, ...) { warning("No function found to write attribute of class '",class(attr),"'. Attribute '",name,"' is not written to hdf5-file.") }

h5writeAttribute.array <- function(attr, h5obj, name, cset=c("ASCII", "UTF8"), variableLengthString=FALSE, asScalar=FALSE) {
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
  h5createAttribute(h5obj, name, dims = dims, storage.mode = storage.mode(attr), size = size, cset=match.arg(cset))
  h5attr <- H5Aopen(h5obj, name)

  DimMem <- dim(attr)
  h5spaceMem <- H5Screate_simple(DimMem,NULL)

  try( { res <- H5Awrite(h5attr, attr) } )

  try( { H5Sclose(h5spaceMem) } )
  try( { H5Aclose(h5attr) } )
  invisible(res)
}

