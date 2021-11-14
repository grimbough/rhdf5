#' H5R - References to objects and regions
#'
#' @description The `H5R` functions can be used for creating or working with
#'   references to specific objects and data regions in an HDF5 file.
#'
#' @author Mike Smith
#' 
#' @examples
#' library(rhdf5)
#' 
#' ## first we'll create a file with a group named "foo" and a
#' ## 1-dimensional dataset named "baa" inside that group.
#' file_name <- tempfile(fileext = ".h5")
#' h5createFile(file_name)
#' h5createGroup(file = file_name, group = "/foo")
#' h5write(1:100, file=file_name, name="/foo/baa")
#'
#'
#' fid <- H5Fopen(file_name)
#' ref_to_group <- H5Rcreate(fid, name = "/foo")
#' ref_to_dataset <- H5Rcreate(fid, name = "/foo/baa")
#' two_refs <- c(ref_to_group, ref_to_dataset)
#' two_refs
#'
#' ## the size of this dataspace is the number of object references
#' ## we want to store
#' sid <- H5Screate_simple(2)
#' tid <- H5Tcopy(dtype_id = "H5T_STD_REF_OBJ")
#' did <- H5Dcreate(fid, name = "object_refs", dtype_id = tid, h5space = sid)
#' H5Dwrite(did, two_refs)
#' H5Dclose(did)
#' H5Sclose(sid)
#' H5Fclose(fid)
#'
#' @name H5R
NULL


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
  
  space_id <- .Call("_H5Rget_region", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  h5space <- new("H5IdComponent", ID = space_id, native = FALSE)
  return(h5space)
}
