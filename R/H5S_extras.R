#' Select elements of a dataspace using R-style indexing
#'
#' Combines a hyperslab selection specified by `start`, `stride`, `count` and
#' `block` arguments with the current selection for the dataspace represented by
#' `h5space`.
#'
#' @details `H5Sselect_hyperslab` is similar to, but subtly different from,
#'   [H5Scombine_hyperslab()].  The former modifies the selection of the
#'   dataspace provided in the `h5space` argument, while the later returns a new
#'   dataspace with the combined selection.
#'
#' @param h5space [H5IdComponent-class] object representing a dataspace.
#' @param index A list of integer indices. The length of the list corresponds to
#'   the number of dimensions of the HDF5 array. If a list element is `NULL`,
#'   all elements of the respective dimension are selected.
#'
#' @examples
#'
#' ## create a 1 dimensional dataspace
#' sid <- H5Screate_simple(c(10,5,3))
#'
#' ## Select elements that lie in in the rows 1-3, columns 2-4, 
#' ## and the entire 3rd dimension
#' H5Sselect_index(sid, list(1:3, 2:4, NULL))
#' 
#' ## We can check the number of selected points. 
#' ## This should be 27 (3 * 3 * 3)
#' H5Sget_select_npoints(sid)
#'
#' ## always close dataspaces after usage to free resources
#' H5Sclose(sid)
#'
#' @export
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
  if (!h5space@native) {
    start = rev(start)
    count = rev(count)
  }

  .Call("_H5Sselect_index", h5space@ID, start, count, PACKAGE='rhdf5')
  invisible(size)
}

## internal version of H5Sselect_index.  The index_null argument is passed from
## h5readDataset and indicates whether an index was automatically generated as
## a response to NULL being passed.  These will always be a sequence along
## the respective dimension, so we can bypass some of the (slow) checks
.H5Sselect_index <- function( h5space, index, index_null ) {
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
      ## no need to do these things if we're sure it's already sorted & unique
      if(!index_null[i]) {
        if (any(index[[i]] > dim[i])) {
          stop("index exceeds HDF5-array dimension.")
        }
        if (any(index[[i]] <= 0)) {
          stop("negative indices and 0 not supported.")
        }
        ind <- sort(unique(index[[i]]))
        test <- diff(ind) > 1
        I <- c(1, which(test) + 1)
      } else {
        ind <- index[[i]]
        I <- 1
      }
      start[[i]] <- ind[I] - 1
      I <- c(I,length(ind)+1)
      count[[i]] <- I[seq_len(length(I)-1)+1] - I[seq_len(length(I)-1)]
  }
  size = sapply(count, sum)
  if (!h5space@native) {
    start = rev(start)
    count = rev(count)
  }
  
  .Call("_H5Sselect_index", h5space@ID, start, count, PACKAGE='rhdf5')
  invisible(size)
}

## check that the provided index doesn't violate 
## and assumption on the size of the dataset
.validIndex <- function(index, dim) {
    if (any(index > dim)) {
        stop("index exceeds HDF5-array dimension.")
    }
    if (any(index <= 0)) {
        stop("negative indices and 0 not supported.")
    }
}

.H5Sselect_dim <- function( h5space, index ) {
    
    h5checktype(h5space, "dataspace")
    dims <- H5Sget_simple_extent_dims(h5space)$size

    res_dim <- integer(length = length(dims))
    
    starts <- counts <- strides <- blocks <- list()
    
    ## creating the set of hyperslabs in each dimension
    for(i in seq_along(index)) {
        
        if(is.null(index[[i]])) {
            ## null index implies we want everything in this dimension
            res_dim[i] <- dims[i]
            starts[[i]] <- 1
            counts[[i]] <- 1
            strides[[i]] <- 1
            blocks[[i]] <- as.numeric(dims[i])
        } else if (length(index[[i]]) == 0) {
            .validIndex(index[[i]], dims[i])
            res_dim[i] <- 0
            starts[[i]] <- NA
            counts[[i]] <- 0
            strides[[i]] <- 1
            blocks[[i]] <- 1
        } else if (length(index[[i]]) == 1) {
            ## catch the special case of only requesting a single entry
            ## in this dimension. This breaks the loop below in its current form.
            
            .validIndex(index[[i]], dims[i])
            
            res_dim[i] <- 1
            starts[[i]] <- index[[i]][1]
            counts[[i]] <- 1
            strides[[i]] <- 1
            blocks[[i]] <- 1
        } else {
            ## two or more entries from this dim
            
            .validIndex(index[[i]], dims[i])
            
            index_copy <- sort(unique(floor(index[[i]])))
            res_dim[i] <- length(index_copy)
            start <- count <- stride <- block <- NULL
            
            ## selecting the optimal break up of the indices
            ## This is not optimised!  We check lags 1:10 and pick the
            ## value with the fewest number of runs.
            if(length(index_copy) > 1) {
                lag <- which.min(sapply(seq_len(min(10, length(index_copy)-1)), 
                                        FUN = function(i, index_copy) { 
                                            i + length(rle(diff(index_copy, lag = i))$lengths) 
                                        },
                                        index_copy))
            } else { lag <- 1 }
            
            ## HDF 1.10.7 seems to have a bug in combined hyperslabs when 'start' parameters
            ## are not in increasing order. This operation ensures we don't have a 0-th group
            ## that would be used first, but is really our lag-th group
            hyperslab_groups <- seq_along(index_copy) %% lag
            hyperslab_groups[hyperslab_groups == 0] <- lag
            indices <- split(index_copy, hyperslab_groups)
            
            for(j in seq_len(lag)) {
                if(length(indices[[j]]) == 1) {
                    ## no sequence, just a single entry
                    start <- c(start, indices[[j]][1])
                    count <- c(count, 1)
                    stride <- c(stride, 1)
                    ## block is always 1, so define outside of the loop
                } else {
                    ## rle for this subset of indices
                    differences <- rle(diff(indices[[j]]))
                    diff_idx <- 1
                    index_copy_idx <- 1
                    
                    while(index_copy_idx <= length(indices[[j]])) {
                        start <- c(start, indices[[j]][ index_copy_idx ])
                        count <- c(count, differences$lengths[ diff_idx ] + 1)
                        stride <- c(stride, differences$values[ diff_idx ])
                        ## block is always 1, so define outside of the loop
                        
                        index_copy_idx <- index_copy_idx + differences$lengths[ diff_idx ]  + 1
                        diff_idx <- diff_idx + 1
                        if( differences$lengths[ diff_idx ] == 1 && diff_idx < length(differences$lengths) ) {
                            diff_idx <- diff_idx + 1
                        } else {
                            differences$lengths[ diff_idx ] <-  differences$lengths[ diff_idx ] - 1
                        }
                    }
                }
            }
            block <- rep(1, length(start))
            
            starts[[i]] <- start
            counts[[i]] <- count
            strides[[i]] <- stride
            blocks[[i]] <- block
        }
    }
    
    ## create all combinations of hyperslab parameters
    ## using as.matrix() here is more efficient than doing it later
    starts2 <- as.matrix(expand.grid(starts))
    strides2 <- as.matrix(expand.grid(strides))
    counts2 <- as.matrix(expand.grid(counts))
    blocks2 <- as.matrix(expand.grid(blocks))

    for(i in seq_len(nrow(starts2))) {
        op <- ifelse(i == 1, "H5S_SELECT_SET", "H5S_SELECT_OR")
        res <- H5Sselect_hyperslab(h5space, op = op,
                                   start = starts2[i,], stride = strides2[i,],
                                   count = counts2[i,], block = blocks2[i,])
    }

    invisible(res_dim)
}


