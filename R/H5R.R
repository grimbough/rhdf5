#' @export
H5Rdereference <- function(ref, h5loc) {
  
  oid <- .Call("_H5Rdereference", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  obj <- new("H5IdComponent", ID = oid, native = FALSE)
  return(obj)
}

#' @export
H5Rget_name <- function(ref, h5loc) {

  name <- .Call("_H5Rget_name", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  return(name)
}

#' @export
H5Rget_obj_type <- function(ref, h5loc) {
  
  object_type <- .Call("_H5Rget_obj_type", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  return(object_type)
}

#' @export
H5Rcreate <- function(h5loc, name, ref_type = "H5R_OBJECT", h5space = NULL) {
  
  ref_type <- h5checkConstants( "H5R_TYPE", ref_type)
  h5space <- h5checktypeOrNULL(h5space, type = "dataspace")
  if((is.null(h5space)) && (h5const2String("H5R_TYPE", ref_type) == "H5R_DATASET_REGION")) {
    stop("H5R_DATASET_REGION references must be accompanied by a H5 dataspace.")
  }
  
  ptr <- .Call("_H5Rcreate", h5loc@ID, name, ref_type, h5space, PACKAGE = "rhdf5")
  ref <- new("H5Ref", val = ptr, type = ref_type)
  
  return(ref)
}