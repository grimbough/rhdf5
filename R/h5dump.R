
h5loadData <- function(h5loc, L, ...) {
  stopifnot( is( h5loc, "H5file" ) | is( h5loc, "H5group" ) ) 
  if (length(L) > 0) {
    for (i in seq_len(length(L))) {
      if (is.data.frame(L[[i]])) {
        if (L[[i]]$otype == h5constants[["H5O_TYPE"]]["H5O_TYPE_DATASET"]) {
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

h5dump <- function( file, recursive = TRUE, load=TRUE, all=FALSE, objecttype = h5default("H5O_TYPE"), index_type = h5default("H5_INDEX"), order = h5default("H5_ITER"), ...) {
  if (is( file, "H5file" ) | is( file, "H5group" )) {
    h5loc = file
  } else {
    if (is.character(file)) {
      if (file.exists(file)) {
        h5loc <- H5Fopen(file)
      } else {
        message("Can not open file '",file,"'.")
        return(NULL)
      }
    } else {
      stop("file has to be either a valid file or an object of class H5file or H5group.")
    }
  }

  ## if (length(datasetinfo)!=1) stop("'datasetinfo' must be an integer of length 1")
  objecttype <- h5checkConstants( "H5O_TYPE", objecttype )
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
  L <- .Call("_h5dump", h5loc@ID, depth, objecttype, di, index_type, order, PACKAGE='rhdf5')
  if (load) {
    L <- h5loadData( h5loc, L, ...)
  } else {
    L <- h5lsConvertToDataframe(L, all=all)
  }
  if (!is( file, "H5file" ) & !is( file, "H5group" )) {
    H5Fclose(h5loc)
  }

  L
}

