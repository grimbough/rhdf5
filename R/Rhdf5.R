
listHandles <- function() {
  invisible(.Call("_listHandles", PACKAGE='rhdf5'))
}

getDatatypeName <- function(type) {
  if (is(type, "H5dataset")) {
    type <- .Call("_handleInfo", 3L, type@ID, PACKAGE='rhdf5')$dtype
  } else {
    type <- as.integer(type)
  }
  .Call("_getDatatypeName", type, PACKAGE='rhdf5')
}

getDatatypeClass <- function(type) {
  if (is(type, "H5dataset")) {
    type <- .Call("_handleInfo", 3L, type@ID, PACKAGE='rhdf5')$dtype
  } else {
    type <- as.integer(type)
  }
  .Call("_getDatatypeClass", type, PACKAGE='rhdf5')
}

