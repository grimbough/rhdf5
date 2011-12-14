
h5write <- function(obj, file, name, ...) {
  res <- UseMethod("h5write")
  invisible(res)
}

h5write.default <- function(obj, file, name, write.attributes = TRUE, ...) {
  if (is( file, "H5file" ) | is( file, "H5group" )) {
    h5loc = file
  } else {
    if (is.character(file)) {
      if (file.exists(file)) {
        try( { h5loc <- H5Fopen(file) } )
      } else {
        stop("Can not open file '",file,"'.")
      }
    } else {
      stop("File has to be either a valid file or an object of class H5file or H5group.")
    }
  }
  res <- h5writeDataset(obj, h5loc, name, ...)
  if (write.attributes) {
    type = H5Oget_info_by_name(h5loc, name)$type
    if (type == "H5O_TYPE_GROUP") {
      h5obj = H5Gopen(h5loc, name)
    } else {
      if (type == "H5O_TYPE_DATASET") {
        h5obj = H5Dopen(h5loc, name)
      } else {
        stop("Cannot open object of this type")
      }
    }
    Attr <- attributes(obj)
    for (i in seq_len(length(Attr))) {
      h5writeAttribute(Attr[[i]], h5obj, name = names(Attr)[i])
    }
    if (type == "H5O_TYPE_GROUP") {
      H5Gclose(h5obj)
    } else {
      if (type == "H5O_TYPE_DATASET") {
        H5Dclose(h5obj)
      }
    }
  }
  if (!is( file, "H5file" ) & !is( file, "H5group" )) {
    try( { H5Fclose(h5loc) } )
  }
  invisible(res)
}

h5writeDataset <- function(obj, h5loc, name, ...) {
  if (!(is( h5loc, "H5file" ) | is( h5loc, "H5group" ))) {
    stop("h5loc not of class H5file or H5group.")
  }
  res <- UseMethod("h5writeDataset")
  invisible(res)
}

h5writeDataset.data.frame <- function(obj, h5loc, name, level=7, DataFrameAsCompound = TRUE) {
  if (DataFrameAsCompound) {
    if (H5Lexists(h5loc, name)) {
      stop("Cannot write data.frame. Object already exists. Subsetting for compound datatype not supported.")
    }
    if (!is.null(level)) { level = as.integer(level) }
    .Call("_h5writeDataFrame", obj, h5loc@ID, name, level, PACKAGE='rhdf5')
    res <- 0
  } else {
    a <- attr(df,"names")
    if (is.null(a)) {
      attr(df,"names") = sprintf("col%d",seq_len(ncol(df)))
    } else {
      if (any(duplicated(a))) {
        a[duplicated(a)] = sprintf("col%d",seq_len(ncol(df)))[duplicated(a)]
        attr(df,"names") = a
      }
    }
    res <- h5writeDataset.list(obj=obj, h5loc=h5loc, name=name, level=level)
  }
  invisible(res)
}

h5writeDataset.list <- function(obj, h5loc, name, level=7) {
  exists <- try( { H5Lexists(h5loc, name) } )
  if (exists) {
    message("Existing object within HDF5 file cannot be overwritten with a list object. First delete the group or dataset from the HDF5 file.")
    res = 0
  } else {
    N = names(obj)
    newnames = FALSE
    if (is.null(N)) {
      newnames = TRUE
    } else {
      if (any(nchar(N) == 0)) {
        newnames = TRUE
      } else {
        if (length(N) != length(obj)) {
          newnames = TRUE
        }
      }
    }
    if (newnames) {
      N = sprintf("ELT%d",seq_len(length(obj)))
    }
    res = NULL
    h5createGroup(h5loc, name)
    gid = H5Gopen(h5loc, name)
    for (i in seq_len(length(obj))) {
      res = h5write(obj[[i]], gid, N[i])
    }
    H5Gclose(gid)
  }
}

h5writeDataset.matrix <- function(...) { h5writeDataset.array(...) }
h5writeDataset.integer <- function(...) { h5writeDataset.array(...) }
h5writeDataset.double <- function(...) { h5writeDataset.array(...) }
h5writeDataset.logical <- function(...) { h5writeDataset.array(...) }
h5writeDataset.character <- function(...) { h5writeDataset.array(...) }

h5writeDataset.array <- function(obj, h5loc, name, index = NULL, start=NULL, stride=NULL, block=NULL, count=NULL, size=NULL, level=7) {
  if (is.null(dim(obj))) {
    dim(obj) = length(obj)
  }
  exists <- try( { H5Lexists(h5loc, name) } )
  if (!exists) {
    if (is.null(size)) {
      size = NULL
      if (storage.mode(obj) == "character") {
        size = max(nchar(obj))+1
      }
      try( { h5createDataset(h5loc, name, dim(obj), storage.mode = storage.mode(obj), size = size, chunk=dim(obj), level=level) } )
    }
  }
  try ( { h5dataset <- H5Dopen(h5loc, name) } )

  try( { h5spaceFile <- H5Dget_space( h5dataset ) } )
  if (!is.null(index)) {
    s = H5Sget_simple_extent_dims(h5spaceFile)$size
    if (length(index) != length(s)) {
      stop("length of index has to be equal to dimensional extension of HDF5 dataset.")
    }
    for (i in seq_len(length(index))) {
      if (is.null(index[[i]])) {
        index[[i]] = seq_len(s[i])
      }
    }
    try ( { H5Sselect_index ( h5spaceFile, index) } )
    d = sapply(index,length)
    d[d == 0] = dim(obj)[d == 0]
    dim(obj) = d
    I = list()
    for (i in seq_len(length(index))) {
      m <- match(index[[i]],unique(sort(index[[i]])))
      I[[i]] = order(m)
      I[[i]] = I[[i]][!duplicated(m[I[[i]]],fromLast=TRUE)]
    }
    obj <- do.call('[',c(list(obj),I))
  } else {
    if (any(c(!is.null(start), !is.null(stride), !is.null(block), !is.null(count)))) {
      if (is.null(block) & is.null(count)) {
        block = rep(1,length(dim(obj)))
        count = dim(obj)
      }
      try ( { H5Sselect_hyperslab ( h5spaceFile, start=start, stride = stride, count=count, block  = block) } )
    }
  }

  DimMem <- dim(obj)
  if (is.null(DimMem)) { DimMem = length(obj) }
  try( { h5spaceMem <- H5Screate_simple(DimMem,NULL) } )

  try( { res <- H5Dwrite(h5dataset, obj, h5spaceMem = h5spaceMem, h5spaceFile = h5spaceFile) } )

  try( { H5Sclose(h5spaceMem) } )
  try( { H5Sclose(h5spaceFile) } )
  try( { H5Dclose(h5dataset) } )
  invisible(res)
}


