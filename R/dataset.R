#
# HDF5 Dataset Functions
#

hdf5.dataset <- function(group,name,data=NULL,dims=NULL,type=NULL,create=hdf5.default.properties) {
  #Check for various wierdness
  if(is.null(data) && is.null(dims))
    error("you must specify either data to store or dimensions for the dataset")
  if(!is.null(data) && !is.null(dims))
    error("you cannot specify dimensions for the dataset AND data! its not right!")
  d <- dims
  if(is.null(d)) d <- dim(data)
  s <- .Call("HDF_dataset_create_simple",group,name,d,create,type)
  #store the matrix into this dataset
  if(!is.null(data))
    s[1:d[1],1:d[2]] <- data
  invisible(s)
}


#We need the dimensionality construct
dim.hdf5 <- function(x) .Call("HDF_dims", x)
length.hdf5.dataset <- function(x) .Call("HDF_dataset_length", x)
min.hdf5.dataset <- function(x, na.rm=FALSE)
	.External("HDF_dataset_min", x, na.rm)
max.hdf5.dataset <- function(x, na.rm=FALSE)
	.External("HDF_dataset_max", x, na.rm)
range.hdf5.dataset <- function(x, na.rm=FALSE)
	.External("HDF_dataset_range", x, na.rm)

is.finite.default <- .Primitive("is.finite")
is.finite <- function(x, ...) UseMethod("is.finite")
is.finite.hdf5.dataset <- function(x, ...) .Call("HDF_dataset_finite", x)

is.matrix.default <- .Primitive("is.matrix")
is.matrix <- function(x, ...) UseMethod("is.matrix")
is.matrix.hdf5.dataset <- function(x, ...) {
   if(length(dim(x)) == 2 && is.integer(dim(x)) )
	return(TRUE)
   return(FALSE)
}

as.matrix.hdf5.dataset <- function(x)
	invisible(.Call("HDF_dataset_mat_load", x))

as.hdf5.dataset <- function(file, value, name) {
    if( !is.vector(value) && is.null(dim(value)))
       stop("only vectors or arrays can be transformed to HDF5")
    if( missing(name) ) name <- deparse(substitute(value))
    invisible(.Call("HDF_dataset_store", file, value, name))
}


"[.hdf5.dataset" <- function(x, ..., drop = TRUE) {
    mf <- match.call()
    mf$x <- NULL; mf$drop <- NULL
    nsubs <- length(mf) - 1
    subargs <- vector("list", length = nsubs)
    missing.marker <- "R_MissingArg"
    for(i in 1:nsubs) {
        tt <- mf[[i + 1]]
        subargs[[i]] <- if (missing(tt)) missing.marker else
                            eval(as.name(paste("..", i, sep="")))
    }
    .Call("HDF_dataset_select",  x, subargs, drop)

}

"[<-.hdf5.dataset" <- function(x,...,value) {
	mf <- match.call()
        mf$x <- NULL; mf$value<-NULL
        nsubs <- length(mf)-1
	subargs<-vector("list", length=nsubs)
        for(i in 1:nsubs) {
	   tt <- mf[[i+1]]
	   subargs[[i]] <- if(missing(tt)) "R_MissingArg" else
	                       eval(as.name(paste("..", i, sep="")))
        }
	.Call("HDF_subassign", x, subargs, value)
	x
}

print.hdf5.dataset <- function(x, ...) {
	.Call("HDF_dataset_print", x)
	invisible(x)
}

addmemory <- function(set1)
    invisible(.Call("HDF_dataset_addmemory", set1))

freememory <- function(set1)
    invisible(.Call("HDF_dataset_freememory", set1))

hasmemory <-function(set1)
    .Call("HDF_dataset_hasmemory", set1)

swapmemory <- function(set1, set2)
    invisible(.Call("HDF_dataset_swapmemory", set1, set2))

getmemory <- function(set1)
    .Call("HDF_dataset_getmemory", set1)
