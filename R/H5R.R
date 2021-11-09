#' @export
H5Rdereference <- function(ref, h5loc) {
  
  oid <- .Call("_H5Rdereference", h5loc@ID, 1, ref@val, PACKAGE = "rhdf5")
  
  obj <- new("H5IdComponent", ID = oid, native = FALSE)
  return(obj)
  
}