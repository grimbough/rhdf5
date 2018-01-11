
H5Screate <- function( type = h5default("H5S"), native = FALSE ) {
  type <- h5checkConstants( "H5S", type )
  sid <- .Call("_H5Screate", type, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5IdComponent", ID = sid, native = native)
  } else {
    message("HDF5: unable to create data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Sclose <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  invisible(.Call("_H5Sclose", h5space@ID, PACKAGE='rhdf5'))
}

H5Scopy <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  sid <- .Call("_H5Scopy", h5space@ID, PACKAGE='rhdf5')
  if (sid > 0) {
    h5spacenew = new("H5IdComponent", ID = sid, native = .native(h5space))
  } else {
    message("HDF5: unable to copy data space")
    h5spacenew = FALSE
  }
  invisible(h5spacenew)
}

H5Screate_simple <- function( dims, maxdims, native = FALSE ) {
  if (missing(maxdims)) {
    maxdims = dims
  }
  dims <- as.numeric(dims)
  maxdims <- as.numeric(maxdims)
  if (!native) {
    dims <- rev(dims)
    maxdims <- rev(maxdims)
  }
  sid <- .Call("_H5Screate_simple", dims, maxdims, PACKAGE='rhdf5')
  if (sid > 0) {
    h5space = new("H5IdComponent", ID = sid, native = native)
  } else {
    message("HDF5: unable to create simple data space")
    h5space = FALSE
  }
  invisible(h5space)
}

H5Sis_simple<- function( h5space ) {
  h5checktype(h5space, "dataspace")
  as.logical(.Call("_H5Sis_simple", h5space@ID, PACKAGE='rhdf5'))
}

H5Sget_simple_extent_dims <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sget_simple_extent_dims", h5space@ID, PACKAGE='rhdf5')
  if (length(res) > 2 && !.native(h5space)) {
    res$size <- rev(res$size)
    res$maxsize <- rev(res$maxsize)
  }
  res
}

H5Sset_extent_simple <- function( h5space, dims, maxdims ) {
  h5checktype(h5space, "dataspace")
  if (missing(maxdims)) {
    maxdims = dims
  }
  dims <- as.numeric(dims)
  maxdims <- as.numeric(maxdims)
  if (!.native(h5space)){
    dims <- rev(dims)
    maxdims <- rev(maxdims)
  }
  res <- .Call("_H5Sset_extent_simple", h5space@ID, dims, maxdims, PACKAGE='rhdf5')
  invisible(res)
}

H5Sselect_hyperslab <- function( h5space, op = h5default("H5S_SELECT"), start=NULL, stride=NULL, count=NULL, block=NULL ) {
  h5checktype(h5space, "dataspace")
  op <- h5checkConstants( "H5S_SELECT", op )

  dims <- H5Sget_simple_extent_dims( h5space )
  R <- dims$rank
  if (length(start) == 0) {
    start <- rep(1, R)
  } else {
    if (length(start) != R) {
      stop(sprintf("start must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (length(stride) == 0) {
    stride <- rep(1, R)
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
    block <- rep(1, R)
  } else {
    if (length(block) != R) {
      stop(sprintf("block must either be NULL or have length %d (rank of dataspace)",R))
    }
  }

  count <- as.numeric(count)
  block <- as.numeric(block)
  stride <- as.numeric(stride)
  size <- count * block
  start <- start - 1
  if (!.native(h5space)) {
    start <- rev(start)
    stride <- rev(stride)
    count <- rev(count)
    block <- rev(block)
  }

  .Call("_H5Sselect_hyperslab", h5space@ID, op, start, stride, count, block, PACKAGE='rhdf5')
  invisible(size)
}

H5Sselect_index <- function( h5space, index ) {
  h5checktype(h5space, "dataspace")
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
      start[[i]] <- 0
      count[[i]] <- as.numeric(dim[i])
    } else {
      if (any(index[[i]] > dim[i])) {
        stop("index exceeds HDF5-array dimension.")
      }
      if (any(index[[i]] <= 0)) {
        stop("negative indices and 0 not supported.")
      }
      ind <- sort(unique(index[[i]]))
      if (length(ind)) {
        test <- ind[seq_len(length(ind)-1)+1]-1 != ind[seq_len(length(ind)-1)]
        I <- c(1, which(test) + 1)
      } else I <- 1
      start[[i]] <- ind[I] - 1
      I <- c(I,length(ind)+1)
      count[[i]] <- I[seq_len(length(I)-1)+1] - I[seq_len(length(I)-1)]
    }
  }
  size = sapply(count, sum)
  if (!.native(h5space)) {
    start = rev(start)
    count = rev(count)
  }

  .Call("_H5Sselect_index", h5space@ID, start, count, PACKAGE='rhdf5')
  invisible(size)
}


## c(1,which(index[seq_len(length(index)-1)+1]-1 != index[seq_len(length(index)-1)]))
