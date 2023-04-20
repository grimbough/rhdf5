h5GetEnumNames <- function(tid) {
  
  enum_names <- .Call("_h5getEnumNames", tid, PACKAGE = "rhdf5")
  return(enum_names)
  
}