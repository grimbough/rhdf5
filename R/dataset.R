#
# HDF5 Dataset Functions
#

hdf5.dataset <- function(group, name, data=NULL, dims=NULL, type=NULL,
                         create=hdf5.default.properties) {
  if(is.null(data) && is.null(dims))
    stop("you must specify either data to store or dimensions for the dataset")
  if(!is.null(data) && !is.null(dims))
    stop("you must specify one of dims or data")
  if(!is.null(data))
    s <- .Call("HDF_dataset_store", group, data, name, PACKAGE="rhdf5")
   else
    s <- .Call("HDF_dataset_create_simple", group, name, dims, create,
               type, PACKAGE="rhdf5")
  invisible(s)
}


#We need the dimensionality construct
dim.hdf5 <- function(x) .Call("HDF_dims", x, PACKAGE="rhdf5")
length.hdf5.dataset <- function(x) .Call("HDF_dataset_length", x,
                                         PACKAGE="rhdf5")
min.hdf5.dataset <- function(..., na.rm=FALSE)
	.External("HDF_dataset_min", x, na.rm, PACKAGE="rhdf5")
max.hdf5.dataset <- function(..., na.rm=FALSE)
	.External("HDF_dataset_max", x, na.rm, PACKAGE="rhdf5")
range.hdf5.dataset <- function(..., na.rm=FALSE)
	.External("HDF_dataset_range", x, na.rm, PACKAGE="rhdf5")

is.finite.default <- function(x, ...) .Primitive("is.finite")(x)
is.finite <- function(x, ...) UseMethod("is.finite")
is.finite.hdf5.dataset <- function(x, ...) .Call("HDF_dataset_finite",
                                                 x, PACKAGE="rhdf5")

is.matrix.default <- function(x, ...) .Primitive("is.matrix")(x)
is.matrix <- function(x, ...) UseMethod("is.matrix")
is.matrix.hdf5.dataset <- function(x, ...) {
   if(length(dim(x)) == 2 && is.integer(dim(x)) )
	return(TRUE)
   return(FALSE)
}

as.matrix.hdf5.dataset <- function(x)
	invisible(.Call("HDF_dataset_mat_load", x, PACKAGE="rhdf5"))

as.hdf5.dataset <- function(file, value, name) {
    if( !is.vector(value) && is.null(dim(value)))
       stop("only vectors or arrays can be transformed to HDF5")
    if( missing(name) ) name <- deparse(substitute(value))
    invisible(.Call("HDF_dataset_store", file, value, name, PACKAGE="rhdf5"))
}

get.points <- function(d,x,y) .Call("HDF_dataset_select_points",d,x,y,
                                    PACKAGE="rhdf5")

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
    .Call("HDF_dataset_select",  x, subargs, drop, PACKAGE="rhdf5")

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
	.Call("HDF_subassign", x, subargs, value, PACKAGE="rhdf5")
	x
}

print.hdf5.dataset <- function(x, ...) {
	.Call("HDF_dataset_print", x, PACKAGE="rhdf5")
	invisible(x)
}

addmemory <- function(set1)
    invisible(.Call("HDF_dataset_addmemory", set1, PACKAGE="rhdf5"))

freememory <- function(set1)
    invisible(.Call("HDF_dataset_freememory", set1, PACKAGE="rhdf5"))

hasmemory <-function(set1)
    .Call("HDF_dataset_hasmemory", set1, PACKAGE="rhdf5")

swapmemory <- function(set1, set2)
    invisible(.Call("HDF_dataset_swapmemory", set1, set2, PACKAGE="rhdf5"))

getmemory <- function(set1)
    .Call("HDF_dataset_getmemory", set1, PACKAGE="rhdf5")
