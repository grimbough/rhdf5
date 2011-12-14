
setMethod("show",signature="H5file", function(object) {
  cat("HDF5 file.\n\n")
  res <- .Call("_handleInfo", 0L, object@ID, PACKAGE='rhdf5')
  if (is.null(res)) {
    cat("file is closed.\n")
  } else {
    cat("Filename: ", res$name,"\n")
    cat("Group:    ", res$group,"\n")
  }
  ## cat("Filename: ", object@filename)
})

setMethod("show",signature="H5group", function(object) {
  cat("HDF5 group.\n\n")
  res <- .Call("_handleInfo", 1L, object@ID, PACKAGE='rhdf5')
  if (is.null(res)) {
    cat("group is closed.\n")
  } else {
    cat("Name:     ", res$name,"\n")
    cat("Group:    ", res$group,"\n")
    resFile <- .Call("_handleInfo", 0L, res$parentFileID, PACKAGE='rhdf5')
    cat("Filename:  ", resFile$name,"\n")
  }
})

setMethod("show",signature="H5space", function(object) {
  cat("HDF5 data space.\n\n")
  res <- .Call("_handleInfo", 2L, object@ID, PACKAGE='rhdf5')
  if (is.null(res)) {
    cat("data space is closed.\n")
  } else {
    cat("Class:   ", res$group,"\n")
    res2 <- .Call("_H5Sget_simple_extent_dims", object@ID, PACKAGE='rhdf5')
    cat("rank:    ", res2$rank,"\n")
    cat("size:    (", paste(rev(res2$size), collapse=" x "), ")","\n")
    cat("maxsize: (", paste(rev(res2$maxsize), collapse=" x "), ")","\n")
  }
})

setMethod("show",signature="H5dataset", function(object) {
  cat("HDF5 dataset.\n\n")
  res <- .Call("_handleInfo", 3L, object@ID, PACKAGE='rhdf5')
  if (is.null(res)) {
    cat("dataset is closed.\n")
  } else {
    cat("Name:          ", res$name,"\n")

    resFile <-  .Call("_handleInfo", 0L, res$parentFileID, PACKAGE='rhdf5')
    resGroup <- .Call("_handleInfo", res$parentType, res$parentID, PACKAGE='rhdf5')
    cat("Group:         ", resGroup$group,"\n")
    cat("File:          ", resFile$name,"\n")
    cat("Type:          ", getDatatypeName(res$dtypeID),"\n")

    resSpace <- .Call("_H5Sget_simple_extent_dims", res$spaceID, PACKAGE='rhdf5')
    if (is.null(resSpace)) {
      cat("Space:         unknown\n")
    } else {
      cat("Space,   rank: ", resSpace$rank,"\n")
      cat("Space,   size: (", paste(rev(resSpace$size), collapse=" x "), ")","\n")
      cat("Space,maxsize: (", paste(rev(resSpace$maxsize), collapse=" x "), ")","\n")
    }
  }
})

setMethod("show",signature="H5attribute", function(object) {
  cat("HDF5 attribute.\n\n")
  res <- .Call("_handleInfo", 4L, object@ID, PACKAGE='rhdf5')
  if (is.null(res)) {
    cat("attribute is closed.\n")
  } else {
    cat("Attr. Name:     ", res$name,"\n")

    resFile <-  .Call("_handleInfo", 0L, res$parentFileID, PACKAGE='rhdf5')
    resGroup <- .Call("_handleInfo", res$parentType, res$parentID, PACKAGE='rhdf5')
    switch(res$parentType,
           "0" = cat("Attribute of file object\n"),
           "1" = cat("Attribute of group object\n"),
           "2" = cat("Attribute of dataset object\n"))
    cat("Object name:    ", resGroup$name,"\n")
    cat("Object group:    ", resGroup$group,"\n")
    cat("File:           ", resFile$name,"\n")
    cat("Attr. Type:     ", getDatatypeName(res$dtypeID),"\n")

    resSpace <- .Call("_H5Sget_simple_extent_dims", res$spaceID, PACKAGE='rhdf5')
    if (is.null(resSpace)) {
      cat("Attr. Space:         unknown\n")
    } else {
      cat("Attr. Space,   rank: ", resSpace$rank,"\n")
      cat("Attr. Space,   size: (", paste(rev(resSpace$size), collapse=" x "), ")","\n")
      cat("Attr. Space,maxsize: (", paste(rev(resSpace$maxsize), collapse=" x "), ")","\n")
    }

  }
})

