#' @export
H5Rcreate <- function(h5loc, name, ref_type = "H5R_OBJECT", h5space = NULL) {
  
  ref_type <- h5checkConstants( "H5R_TYPE", ref_type)
  h5checktypeOrNULL(h5space, type = "dataspace")
  if(is.null(h5space)) {
    if(h5const2String("H5R_TYPE", ref_type) == "H5R_DATASET_REGION") {
      stop("H5R_DATASET_REGION references must be accompanied by a H5 dataspace.")
    } else {
      ## we pass a dataspace ID of -1 if this is an object reference
      h5space <- new("H5IdComponent", ID = "-1", native = FALSE)
    }
  }
  
  ptr <- .Call("_H5Rcreate", h5loc@ID, name, ref_type, h5space@ID, PACKAGE = "rhdf5")
  ref <- new("H5Ref", val = ptr, type = ref_type)
  
  return(ref)
}

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

#' @param ref An object of class `H5Ref`. 
#' @param h5loc
#' 
#' @return An object of class H5IdComponent representing the dataspace of the dataset 
#' that `ref` points to.  The dataspace will have the selection set that matches the 
#' selection pointed to by `ref`.
#'  
#' @export
H5Rget_region <- function(ref, h5loc) {
  
  if(h5const2String("H5R_TYPE", ref@type) != "H5R_DATASET_REGION") {
    stop("Only references of type H5R_DATASET_REGION can be used.")
  }
  
  h5space <- .Call("_H5Rget_region", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  return(h5space)
}
