#' Create a new dataspace of a specified type
#'
#' @param type The type of dataspace to create. See `h5const("H5S")` for
#'   possible types.
#' @param native An object of class `logical`. If `TRUE`, array-like
#'   objects are treated as stored in HDF5 row-major rather than R column-major
#'   orientation. Using `native = TRUE` increases HDF5 file portability between
#'   programming languages. A file written with `native = TRUE` should also be
#'   read with `native = TRUE`.
#'
#' @return Returns an object of class [H5IdComponent-class] representing a
#'   dataspace.
#'
#' @seealso [H5Screate_simple]
#'
#' @export
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


#' Create a simple dataspace
#'
#' @param dims A numeric vector defining the initial dimensions of the dataspace.
#' The length of `dims` determines the rank of the dataspace.
#' @param maxdims A numeric vector with the same length length as `dims`.  Specifies the
#' upper limit on the size of the dataspace dimensions.  Only needs to be specified
#' if this is different from the values given to `dims`.
#' @param native An object of class `logical`. If `TRUE`, array-like
#'   objects are treated as stored in HDF5 row-major rather than R column-major
#'   orientation. Using `native = TRUE` increases HDF5 file portability between
#'   programming languages. A file written with `native = TRUE` should also be
#'   read with `native = TRUE`.
#'
#' @return Returns an object of class [H5IdComponent-class] representing a
#'   dataspace.
#'
#' @seealso [H5Screate]
#'
#' @export
H5Screate_simple <- function( dims, maxdims, native = FALSE ) {
  dims <- as.numeric(dims)
  if (missing(maxdims)) {
    maxdims <- dims
  } else {
    maxdims <- as.numeric(maxdims)
  }
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

#' Close and release a dataspace
#' 
#' @param h5space Object of class [H5IdComponent-class] representing the 
#' dataspace to be closed.
#' 
#' @seealso [H5Screate()]
#' 
#' @export
H5Sclose <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  invisible(.Call("_H5Sclose", h5space@ID, PACKAGE='rhdf5'))
}

#' Create a copy of a dataspace
#' 
#' `H5S_copy()` creates an exact copy of a given dataspace.
#' 
#' @param h5space Object of class [H5IdComponent-class] representing the 
#' dataspace to be copied.
#' 
#' @return If the copying is successful returns an object of class 
#' [H5IdComponent-class] representing the new dataspace.  Otherwise returns
#' `FALSE`.
#' 
#' @export
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



#' Determine whether a dataspace is a simple dataspace
#' 
#' In HDF5 a dataspace is considered "simple" if it represents a regular
#' N-dimensional array of points.
#' Currently (HDF 1.10.7) all dataspaces are simple.  Support for complex
#' dataspaces is planned for future HDF versions.
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' 
#' @export
H5Sis_simple<- function( h5space ) {
  h5checktype(h5space, "dataspace")
  as.logical(.Call("_H5Sis_simple", h5space@ID, PACKAGE='rhdf5'))
}

#' Find the size of a dataspace
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' 
#' @export
H5Sget_simple_extent_dims <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sget_simple_extent_dims", h5space@ID, PACKAGE='rhdf5')
  if (length(res) > 2 && !h5space@native) {
    res$size <- rev(res$size)
    res$maxsize <- rev(res$maxsize)
  }
  res
}

#' Set the size of a dataspace
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' @param dims Dimension of the dataspace. This argument is similar to the dim
#' attribute of an array. When viewing the HDF5 dataset with an C-program 
#' (e.g. HDFView), the dimensions appear in inverted order, because the 
#' fastest changing dimension in R is the first one, and in C its the last 
#' one.
#' @param maxdims Maximum extension of the dimension of the dataset in the 
#' file. If not provided, it is set to `dims`.
#' 
#' @export
H5Sset_extent_simple <- function( h5space, dims, maxdims) {
  h5checktype(h5space, "dataspace")
  
  dims <- as.numeric(dims)
  if (missing(maxdims)) {
    maxdims <- dims
  } else {
    maxdims <- as.numeric(maxdims)
  }
  
  if (!h5space@native){
      dims <- rev(dims)
      maxdims <- rev(maxdims)
  }
  res <- .Call("_H5Sset_extent_simple", h5space@ID, dims, maxdims, PACKAGE='rhdf5')
  invisible(res)
}

#' Find the number of elements in a dataspace selection
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' 
#' @export
H5Sget_select_npoints <- function( h5space ) {
  h5checktype(h5space, "dataspace")
  res <- .Call('_H5Sget_select_npoints', h5space@ID, PACKAGE = "rhdf5")
  return(res)
}

#' Set the selection region of a dataspace to include all elements
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' 
#' @export
H5Sselect_all <- function(h5space) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sselect_all", h5space@ID, PACKAGE='rhdf5')
  invisible(res)
}

#' Set the selection region of a dataspace to include no elements
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' 
#' @export
H5Sselect_none <- function(h5space) {
  h5checktype(h5space, "dataspace")
  res <- .Call("_H5Sselect_none", h5space@ID, PACKAGE='rhdf5')
  invisible(res)
}

#' Check that a selection is valid
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' 
#' @export
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

#' Perform operation between an existing selection and an another 
#' hyperslab definition.
#' 
#' Combines a hyperslab selection specified by `start`, `stride`, `count` and 
#' `block` arguments with the current selection for the dataspace 
#' represented by `h5space`.
#' 
#' @details `H5Sselect_hyperslab` is similar to, but subtly different from, 
#' [H5Scombine_hyperslab()].  The former modifies the selection of the 
#' dataspace provided in the `h5space` argument, while the later returns a
#' new dataspace with the combined selection.
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' @param op Character string defined the operation used to join the two 
#' dataspaces.  See `h5const("H5S_SELECT")` for the list of available options.
#' @param start,stride,count,block Integer vectors, each with length equal
#' to the rank of the dataspace.  These parameters define the new hyperslab
#' to select.
#' 
#' @examples 
#' 
#' ## create a 1 dimensional dataspace
#' sid_1 <- H5Screate_simple(dims = 20)
#' 
#' ## select a single block of 5 points in sid_1
#' ## this is equivalent to [11:16] in R syntax
#' H5Sselect_hyperslab(sid_1, start = 11, stride = 1, 
#'                     block = 5, count = 1)
#'                     
#' ## confirm we have selected 5 in our original dataspace
#' H5Sget_select_npoints(sid_1)
#' 
#' ## combine the existing selection with a new
#' ## selection consisting of 2 blocks each of 1 point
#' ## equivalent to [c(3,5)] in R syntax
#' H5Sselect_hyperslab(sid_1, op = "H5S_SELECT_OR",
#'                      start = 3, stride = 2, 
#'                      block = 1, count = 2)
#' 
#' ## The dataspace now has 7 points selected
#' H5Sget_select_npoints(sid_1)
#' 
#' ## tidy up
#' H5Sclose(sid_1)
#' 
#' @export
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

#' Perform operation between an existing selection and an another 
#' hyperslab definition.
#' 
#' Combines a hyperslab selection specified by `start`, `stride`, `count` and 
#' `block` arguments with the current selection for the dataspace 
#' represented by `h5space`.
#' 
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' @param op Character string defined the operation used to join the two 
#' dataspaces.  See `h5const("H5S_SELECT")` for the list of available options.
#' @param start,stride,count,block Integer vectors, each with length equal
#' to the rank of the dataspace.  These parameters define the new hyperslab
#' to select.
#'  
#' @returns An [H5IdComponent-class] object representing a new dataspace 
#' with the generated selection.
#' 
#' @examples 
#' 
#' ## create a 1 dimensional dataspace
#' sid_1 <- H5Screate_simple(dims = 20)
#' 
#' ## select a single block of 5 points in sid_1
#' ## this is equivalent to [11:16] in R syntax
#' H5Sselect_hyperslab(sid_1, start = 11, stride = 1, 
#'                     block = 5, count = 1)#
#' 
#' ## combine the existing selection with a new
#' ## selection consisting of 2 blocks each of 1 point
#' ## equivalent to [c(3,5)] in R syntax
#' sid_2 <- H5Scombine_hyperslab(sid_1, op = "H5S_SELECT_OR",
#'                               start = 3, stride = 2, 
#'                               block = 1, count = 2)
#' 
#' ## confirm we have selected 5 in our original dataspace
#' ## and 7 points in the newly created dataspace
#' H5Sget_select_npoints(sid_1)
#' H5Sget_select_npoints(sid_2)
#' 
#' ## tidy up
#' H5Sclose(sid_1)
#' H5Sclose(sid_2)
#' 
#' @seealso [H5Scombine_select()], [H5Sselect_hyperslab()]
#' 
#' @export
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

#' Combine two selections
#' 
#' @param h5space1,h5space2 [H5IdComponent-class] objects representing a dataspaces.
#' @param op Character string defined the operation used to join the two 
#' dataspaces.  See `h5const("H5S_SELECT")` for the list of available options.
#' 
#' @return Returns an [H5IdComponent-class] object representing a new dataspace.
#' The new dataspace will have the same extent as `h5space1` with the 
#' hyperslab selection being the result of combining the selections of 
#' `h5space1` and `h5space2`.
#' 
#' @examples 
#' 
#' ## create two 1 dimensional dataspaces
#' ## of different sizes
#' sid_1 <- H5Screate_simple(dims = 20)
#' sid_2 <- H5Screate_simple(dims = 10)
#' 
#' ## select a single block of 5 points in sid_1
#' ## this is equivalent to [11:16] in R syntax
#' H5Sselect_hyperslab(sid_1, start = 11, stride = 1, 
#'                     block = 5, count = 1)
#' 
#' ## select 2 blocks of 1 point from sid_2
#' ## equivalent to [c(3,5)] in R syntax
#' H5Sselect_hyperslab(sid_2, start = 3, stride = 2, 
#'                     block = 1, count = 2)
#' 
#' ## confirm we have select 5 and 2 points resepectively
#' H5Sget_select_npoints(sid_1)
#' H5Sget_select_npoints(sid_2)
#' 
#' ## combine the two dataset selections keeping points that
#' ## are in one or both of the selections
#' sid_3 <- H5Scombine_select(sid_1, "H5S_SELECT_OR", sid_2)
#' 
#' ## extent of the new dataset is the same as sid_1
#' sid_3
#' ## confirm the selection contains 7 points
#' H5Sget_select_npoints(sid_3)
#' 
#' ## tidy up
#' H5Sclose(sid_1)
#' H5Sclose(sid_2)
#' H5Sclose(sid_3)
#' 
#' @seealso [H5Scombine_hyperslab()]
#' 
#' @export
H5Scombine_select <- function( h5space1, op = h5default("H5S_SELECT"), h5space2 ) {
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

#' Retrieve value for `H5S_UNLIMITED` constant
#' 
#' The value for `H5S_UNLIMITED` can be provided to the `maxdims` argument of [H5Screate_simple] 
#' to indicate that the maximum size of the corresponding dimension is unlimited.
#' 
#' @seealso [H5Screate_simple]
#' 
#' @export
H5Sunlimited <- function()  {
  #as.integer(h5checkConstants("H5S_UNLIMITED", "H5S_UNLIMITED"))
  as.integer(-1)
}




