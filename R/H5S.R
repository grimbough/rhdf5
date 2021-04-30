
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
    h5spacenew = new("H5IdComponent", ID = sid, native = h5space@native)
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
  if (length(res) > 2 && !h5space@native) {
    res$size <- rev(res$size)
    res$maxsize <- rev(res$maxsize)
  }
  res
}

H5Sset_extent_simple <- function( h5space, dims, maxdims) {
  h5checktype(h5space, "dataspace")
  if (missing(maxdims)) {
    maxdims = dims
  }
  dims <- as.numeric(dims)
  maxdims <- as.numeric(maxdims)
  if (!h5space@native){
      dims <- rev(dims)
      maxdims <- rev(maxdims)
  }
  res <- .Call("_H5Sset_extent_simple", h5space@ID, dims, maxdims, PACKAGE='rhdf5')
  invisible(res)
}

H5Sget_select_npoints <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  res <- .Call('_H5Sget_select_npoints', h5space@ID, PACKAGE = "rhdf5")
  return(res)
}

H5Sselect_all <- function(h5space) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sselect_all", h5space@ID, PACKAGE='rhdf5')
  invisible(res)
}

H5Sselect_none <- function(h5space) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sselect_none", h5space@ID, PACKAGE='rhdf5')
  invisible(res)
}

H5Sselect_valid <- function(h5space) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sselect_valid", h5space@ID, PACKAGE='rhdf5')
  invisible(res)
}

H5Sselect_elements <- function( h5space, op = h5default("H5S_SELECT"), index) {
  
  op <- h5checkConstants( "H5S_SELECT", op )
  
  dims <- H5Sget_simple_extent_dims( h5space )
  
  if(length(index) != dims$rank) {
    stop("Index must be the same length as the rank of the dataspace.")
  }
  
  if(h5space@native) {
    coords <- expand.grid(index)
  } else {
    coords <- rev(expand.grid(index))
  }
  
  coords <- as.integer(t(coords))
  numElements <- as.integer(length(coords) / dims$rank)
  
  res <- .Call("_H5Sselect_elements", h5space@ID, op, numElements, coords, PACKAGE='rhdf5')
  if(res < 0) { 
    stop("Error selecting elements")
  }
  
  size <- H5Sget_select_npoints(h5space)
  return(size)
}

H5Sselect_hyperslab <- function( h5space, op = h5default("H5S_SELECT"), 
                                 start=NULL, stride=NULL, count=NULL, block=NULL ) {
  h5checktype(h5space, "dataspace")
  op <- h5checkConstants( "H5S_SELECT", op )

  dims <- H5Sget_simple_extent_dims( h5space )
  R <- dims$rank
  if (is.null(start)) {
    start <- rep(1, R)
  } else {
    if (length(start) != R) {
      stop(sprintf("start must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (is.null(stride)) {
    stride <- rep(1, R)
  } else {
    if (length(stride) != R) {
      stop(sprintf("stride must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (is.null(count)) {
    count <- dims$size
  } else {
    if (length(count) != R) {
      stop(sprintf("count must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (is.null(block)) {
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
  if (!h5space@native) {
    start <- rev(start)
    stride <- rev(stride)
    count <- rev(count)
    block <- rev(block)
  }

  res <- .Call("_H5Sselect_hyperslab", h5space@ID, op, start, stride, count, block, PACKAGE='rhdf5')
  if(res < 0) { stop("Error selecting hyperslab") }
  invisible(size)
}

H5Scombine_hyperslab <- function( h5space, op = h5default("H5S_SELECT"), 
                                  start=NULL, stride=NULL, count=NULL, block=NULL ) {
  
  h5checktype(h5space, "dataspace")
  op <- h5checkConstants( "H5S_SELECT", op )
  
  dims <- H5Sget_simple_extent_dims( h5space )
  R <- dims$rank
  if (is.null(start)) {
    start <- rep(1, R)
  } else {
    if (length(start) != R) {
      stop(sprintf("start must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (is.null(stride)) {
    stride <- rep(1, R)
  } else {
    if (length(stride) != R) {
      stop(sprintf("stride must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (is.null(count)) {
    count <- dims$size
  } else {
    if (length(count) != R) {
      stop(sprintf("count must either be NULL or have length %d (rank of dataspace)",R))
    }
  }
  if (is.null(block)) {
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
  if (!h5space@native) {
    start <- rev(start)
    stride <- rev(stride)
    count <- rev(count)
    block <- rev(block)
  }
  
  sid <- .Call("_H5Scombine_hyperslab", h5space@ID, op, start, stride, count, block, PACKAGE='rhdf5')
  if (sid > 0) {
    h5spacenew = new("H5IdComponent", ID = sid, native = h5space@native)
  } else {
    message("HDF5: error combining hyperslabs")
    h5spacenew = FALSE
  }
  
  invisible(h5spacenew)
  
}

H5Scombine_select <- function( h5space1, op, h5space2 ) {
  h5checktype(h5space1, "dataspace")
  h5checktype(h5space2, "dataspace")
  op <- h5checkConstants( "H5S_SELECT", op )
  
  sid <- .Call("_H5Scombine_select", h5space1@ID, op, h5space2@ID)
  
  if (sid > 0) {
    h5spacenew = new("H5IdComponent", ID = sid, native = h5space1@native)
  } else {
    message("HDF5: unable to copy data space")
    h5spacenew = FALSE
  }
  invisible(h5spacenew)
  
}

H5Sunlimited <- function()  {
  as.integer(h5checkConstants("H5S_UNLIMITED", "H5S_UNLIMITED"))
}




