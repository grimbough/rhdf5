h5getEnumNames <- function(tid) {
  
  enum_names <- .Call("_h5getEnumNames", tid, PACKAGE = "rhdf5")
  return(enum_names)
  
}

h5getEnumValues <- function(tid) {
  
  enum_vals <- .Call("_h5getEnumValues", tid, PACKAGE = "rhdf5")
  return(enum_vals)
  
}