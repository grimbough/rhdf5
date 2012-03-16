
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

h5writeAttribute.array <- function(attr, h5obj, name, size=NULL) {
  if (is.null(dim(attr))) {
    dim(attr) = length(attr)
  }
  if (H5Aexists(h5obj, name)) {
    H5Adelete(h5obj, name)
  }
  size = NULL
  if (storage.mode(attr) == "character") {
    size = max(nchar(attr))+1
  }
  h5createAttribute(h5obj, name, dims = dim(attr), storage.mode = storage.mode(attr), size=size)
  h5attr <- H5Aopen(h5obj, name)

  DimMem <- dim(attr)
  h5spaceMem <- H5Screate_simple(DimMem,NULL)

  try( { res <- H5Awrite(h5attr, attr) } )

  try( { H5Sclose(h5spaceMem) } )
  try( { H5Aclose(h5attr) } )
  invisible(res)
}

