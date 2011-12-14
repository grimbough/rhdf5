
H5Screate <- function( type = h5default("H5S") ) {
  type <- h5checkConstants( "H5S", type )
  sid <- .Call("_H5Screate", type, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5space", ID = sid)
  } else {
    message("HDF5: unable to create data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Sclose <- function( h5space ) {
  stopifnot( is( h5space, "H5space" ) )
  invisible(.Call("_H5Sclose", h5space@ID, PACKAGE='rhdf5'))
}

H5Scopy <- function( h5space ) {
  stopifnot( is( h5space, "H5space" ) )
  sid <- .Call("_H5Scopy", h5space@ID, PACKAGE='rhdf5')
  if (sid > 0) {
    h5spacenew = new("H5space", ID = sid)
  } else {
    message("HDF5: unable to copy data space")
    h5spacenew = FALSE
  }
  invisible(h5spacenew)
}

H5Screate_simple <- function( dims, maxdims = dims) {
  sid <- .Call("_H5Screate_simple", as.integer(dims), as.integer(maxdims), PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5space", ID = sid)
  } else {
    message("HDF5: unable to create simple data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Sis_simple<- function( h5space ) {
  stopifnot( is( h5space, "H5space" ) )
  as.logical(.Call("_H5Sis_simple", h5space@ID, PACKAGE='rhdf5'))
}

H5Sget_simple_extent_dims <- function( h5space ) {
  stopifnot( is( h5space, "H5space" ) )
  res <- .Call("_H5Sget_simple_extent_dims", h5space@ID, PACKAGE='rhdf5')
  if (length(res) > 2) {
    res$size <- rev(res$size)
    res$maxsize <- rev(res$maxsize)
  }
  res
}

H5Sselect_hyperslab <- function( h5space, op = h5default("H5S_SELECT"), start=NULL, stride=NULL, count=NULL, block=NULL ) {
  stopifnot( is( h5space, "H5space" ) )
  op <- h5checkConstants( "H5S_SELECT", op )

  dims <- H5Sget_simple_extent_dims( h5space )
  R <- dims$rank
  if (length(start) == 0) {
    start <- rep(1L, R)
  } else {
    if (length(start) != R) {
      stop(sprintf("start must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (length(stride) == 0) {
    stride <- rep(1L, R)
  } else {
    if (length(stride) != R) {
      stop(sprintf("stride must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (length(count) == 0) {
    count <- dims$size
  } else {
    if (length(count) != R) {
      stop(sprintf("count must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (length(block) == 0) {
    block <- rep(1L, R)
  } else {
    if (length(block) != R) {
      stop(sprintf("block must either be NULL or have length %d (rank of dataspace)",R))
    }
  }

  size <- as.integer(count) * as.integer(block)
  start <- start - 1
  start <- rev(start)
  stride <- rev(stride)
  count <- rev(count)
  block <- rev(block)

  start = as.integer(start)
  stride = as.integer(stride)
  count = as.integer(count)
  block = as.integer(block)
  .Call("_H5Sselect_hyperslab", h5space@ID, op, start, stride, count, block, PACKAGE='rhdf5')
  invisible(size)
}

H5Sselect_index <- function( h5space, index ) {
  stopifnot( is( h5space, "H5space" ) )
  dim <- H5Sget_simple_extent_dims(h5space)$size
  if (!is.list(index)) {
    index = list(index)
  }

  if (length(index) != length(dim)) {
    stop("length of list index not equal to h5space dimensional extension.")
  }

  start <- list()
  count <- list()
  for (i in seq_len(length(index))) {
    if (is.null(index[[i]])) {
      ## index[[i]] <- seq_len(dim[i]) - 1L
      start[[i]] <- 0L
      count[[i]] <- as.integer(dim[i])
    } else {
      index[[i]] <- as.integer(index[[i]])
      if (any(index[[i]] > dim[i])) {
        stop("index exceeds HDF5-array dimension.")
      }
      if (any(index[[i]] <= 0)) {
        stop("negative indices and 0 not supported.")
      }
      ind <- sort(unique(index[[i]]))
      I <- c(1,which(ind[seq_len(length(ind)-1)+1]-1 != ind[seq_len(length(ind)-1)])+1)
      start[[i]] <- as.integer(ind[I] - 1L)
      I <- c(I,length(ind)+1)
      count[[i]] <- as.integer(I[seq_len(length(I)-1)+1] - I[seq_len(length(I)-1)])
    }
  }
  size = sapply(count, sum)
  start = rev(start)
  count = rev(count)

  .Call("_H5Sselect_index", h5space@ID, start, count, PACKAGE='rhdf5')
  invisible(size)
}


## c(1,which(index[seq_len(length(index)-1)+1]-1 != index[seq_len(length(index)-1)]))
