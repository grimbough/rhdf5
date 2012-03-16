
setMethod("show",signature="H5IdComponent", function(object) {
  res <- .Call("_handleInfo", object@ID, PACKAGE='rhdf5')
  res$type = as.character(h5const2Factor("H5I_TYPE",res$type))
  res$type = substr(res$type,5,1000)
  cat(sprintf("HDF5 %s\n\n",res$type))
  if (res$type == "DATASPACE") {
    res2 <- .Call("_H5Sget_simple_extent_dims", object@ID, PACKAGE='rhdf5')
    res$info = c(res$info,
      rank = res2$rank,
      size = paste(rev(res2$size), collapse=" x "),
      maxsize = paste(rev(res2$maxsize), collapse=" x "))
  }
  if (res$type == "DATASET") {
    s = H5Dget_space(object)
    res2 <- .Call("_H5Sget_simple_extent_dims", s@ID, PACKAGE='rhdf5')
    H5Sclose(s)
    res$info = c(res$info,
      type =  .Call("_getDatatypeName", H5Dget_type(object), PACKAGE='rhdf5'),
      rank = res2$rank,
      size = paste(rev(res2$size), collapse=" x "),
      maxsize = paste(rev(res2$maxsize), collapse=" x "))
  }
  if (res$type == "ATTR") {
    s = H5Aget_space(object)
    res2 <- .Call("_H5Sget_simple_extent_dims", s@ID, PACKAGE='rhdf5')
    H5Sclose(s)
    names(res$info)[names(res$info) == "name"] = "objName"
    res$info = c(res$info,
      attrName = H5Aget_name(object),
      type = .Call("_getDatatypeName", H5Aget_type(object), PACKAGE='rhdf5'),
      rank = res2$rank,
      size = paste(rev(res2$size), collapse=" x "),
      maxsize = paste(rev(res2$maxsize), collapse=" x "))
  }
  if (res$type == "DATATYPE") {
    res$info = c(res$info,
      type = .Call("_getDatatypeName", object@ID, PACKAGE='rhdf5'))
  }
  if (!is.null(res$info)) {
    cat(paste(sprintf("%12s %s\n",names(res$info),res$info),collapse=""))
  }
  if (!res$isvalid) {
    cat("\nNo valid identifier.\n")
  }
})

