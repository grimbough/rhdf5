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


#' Create a reference
#' 
#' Creates a reference to an object or dataset selection inside an HDF5 file.
#' 
#' @param h5loc An `H5IdComponent` object representing the location to be pointed to
#' by the created reference.
#' @param name Character string giving the name of the object to be referenced, 
#' relative to the location given by `h5loc`.
#' @param ref_type The type of reference to create.  Accepts either `H5R_OBJECT`
#' or `H5R_DATASET_REGION`.
#' @param h5space An object of class `H5IdComponent` representing a dataspace
#' with a selection set.  This argument is only used if creating a 
#' reference to a dataset region, and will be ignored otherwise.
#' 
#' @return An [H5Ref-class] object storing the reference. 
#' 
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

#' Open a reference object.
#'
#' Given a reference and the file to which that reference applies,
#' `H5Rdeference()` will open the reference object and return an identifier.
#'
#' @details If `ref` contains more than one reference, only the first reference
#'   will be used.  It must be subset with `[` if one of the other stored
#'   references should be opened.
#'
#' @param ref `H5ref` object containing the reference to be opened.
#' @param h5loc An `H5IdComponent` object representing the file containing the
#'   referenced object.
#'
#' @return An object of class `H5IdComponent` representing the opened object
#'   referenced by `ref`.  This should be closed with the appropriate function
#'   e.g. [H5Dclose()], [H5Oclose()], etc. when no longer needed.
#'
#' @export
H5Rdereference <- function(ref, h5loc) {
  
  oid <- .Call("_H5Rdereference", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  obj <- new("H5IdComponent", ID = oid, native = FALSE)
  return(obj)
}

#' Return the name of the object that a reference points to
#'
#' @param ref `H5ref` object containing the reference to be queried.
#' @param h5loc An `H5IdComponent` object representing the file containing the
#'   referenced object.
#'
#' @return Character string of length 1 giving the name of the referenced
#'   object.
#'
#' @export
H5Rget_name <- function(ref, h5loc) {

  name <- .Call("_H5Rget_name", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  return(name)
}

#' Identify the type of object that a reference points to
#' 
#' @param ref `H5ref` object containing the reference to be queried.
#' @param h5loc An `H5IdComponent` object representing the file containing the
#'   referenced object.
#'   
#' @return Character string of length 1 identifying the object type.  Valid return
#' values are: `"GROUP"`, `"DATASET"`, and `"NAMED_DATATYPE"`.
#' 
#' @export
H5Rget_obj_type <- function(ref, h5loc) {
  
  object_type <- .Call("_H5Rget_obj_type", h5loc@ID, ref@type, ref@val, PACKAGE = "rhdf5")
  return(object_type)
}

#' Return selection for a reference to dataset region
#' 
#' Given a dataset region reference, this function will return the dataspace
#' and selection required to read the data points indicated by the reference.
#' 
#' @param ref An object of class `H5Ref`.  This function is only valid
#' for reference of type `H5R_DATASET_REGION`, and not `H5R_OBJECT`.
#' @param h5loc An `H5IdComponent` object representing the file containing the
#'   referenced object.
#'
#' @return An object of class `H5IdComponent` representing the dataspace of the
#'   dataset that `ref` points to.  The dataspace will have the selection set
#'   that matches the selection pointed to by `ref`. This should be closed using
#'   [H5Sclose()] when no longer required.
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
