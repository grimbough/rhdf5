
h5loadData <- function(h5loc, L, ...) {
  h5checktype(h5loc,"loc")
  if (length(L) > 0) {
    for (i in seq_len(length(L))) {
      if (is.data.frame(L[[i]])) {
        if (L[[i]]$otype == h5constants[["H5I_TYPE"]]["H5I_DATASET"]) {
          L[i] = list(h5read( h5loc, L[[i]]$name, ...))
        }
      } else {
        group = H5Gopen(h5loc, names(L)[i])
        L[i] = list(h5loadData(group, L[[i]], ...))
        H5Gclose(group)
      }
    }
  }
  L
}

h5dump <- function( file, recursive = TRUE, load=TRUE, all=FALSE, index_type = h5default("H5_INDEX"), order = h5default("H5_ITER"), ...) {
  loc = h5checktypeOrOpenLoc(file)

  index_type <- h5checkConstants( "H5_INDEX", index_type )
  order <- h5checkConstants( "H5_ITER", order )
  if (is.logical(recursive)) {
    if (recursive) {
      depth = -1L
    } else {
      depth = 1L
    }
  } else {
    if (is.numeric(recursive)) {
      depth = as.integer(recursive)
      if (recursive == 0) {
        stop("value 0 for 'recursive' is undefined, either a positive integer or negative (maximum recursion)")
      }
    } else {
      stop("'recursive' must be an integer of length 1 or a logical")
    }
  }
  di <- as.integer(2)
  L <- .Call("_h5dump", loc$H5Identifier@ID, depth, index_type, order, PACKAGE='rhdf5')
  if (load) {
    L <- h5loadData( loc$H5Identifier, L, ...)
  } else {
    L <- h5lsConvertToDataframe(L, all=all)
  }
  h5closeitLoc(loc)

  L
}

