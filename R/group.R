#
# HDF5 Group Related Functions
#

hdf5.group <- function(group, name)
  invisible(.Call("HDF_group_mkgroup", group,name))
hdf5.group.rm <- function(group, name) {
  .Call("HDF_group_delete",group, name)
  invisible(group)
}
print.hdf5.group <- function(x, ...) {
  .Call("HDF_group_print", x)
  invisible(x)
}
names.hdf5.group <- function(x, ...)
  .Call("HDF_group_members", x)

"[[.hdf5.group" <- function(group, name) {
  info <- .Call("HDF_group_get_info", group, name)
  if( is.null(info)) return(NULL)
  if(info$type == 1) { #group
    return(.Call("HDF_group_get_group", group, name))
  }
  if(info$type == 2) { #dataset
    return(.Call("HDF_group_get_dataset", group, name))
  }
  return(NULL)
}
"$.hdf5.group" <- function(group,name) "[[.hdf5.group"(group,name)
"[.hdf5.group" <- function(x,...) "[[.hdf5.group"(x,names(x)[..1])

#Why doesn't this work?
as.container <- function(x) UseMethod("as.container")
as.container.hdf5.group <- function(group) {
  #There should be some consistency checking here
  class(group) <- c("container",class(group))
  group
}
hdf5.apply <- function(X, REGION, FUN=function(x) x,...) {
  FUN <- match.fun(FUN)
  .Call("HDF_group_apply",X,FUN,REGION,list(...))
}





