#
# HDF5 Group Related Functions
#

hdf5.group <- function(group, name)
  invisible(.Call("HDF_group_mkgroup", group,name, PACKAGE="rhdf5"))
hdf5.group.rm <- function(group, name) {
  .Call("HDF_group_delete",group, name, PACKAGE="rhdf5")
  invisible(group)
}
print.hdf5.group <- function(x, ...) {
  .Call("HDF_group_print", x, PACKAGE="rhdf5")
  invisible(x)
}

names.hdf5.group <- function(x, ...)
  .Call("HDF_group_members", x, PACKAGE="rhdf5")

print.hdf5.info <- function(x, ...) {
  cat("Type: ", x$type, "\n")
  cat("Links: ", x$links, "\n")
  cat("Modification Time: ", x$modification.time, "\n")
  invisible(x)
}
groupgetinfo <- function(group, name)
  .Call("HDF_group_get_info", group, name, PACKAGE="rhdf5")

"[[.hdf5.group" <- function(group, name) {
    if(length(name) != 1)
        stop("second argument is the wrong length")
    nms <- names(group)
    if(is.numeric(name) ) {
        if(length(group) < name)
            stop("index out of range")
        name <- nms[name]
    } else {
        if( !(name %in% nms) )
            stop(paste(name, "is not one of the named objects"))
    }

   info <- .Call("HDF_group_get_info", group, name, PACKAGE="rhdf5")
   if( is.null(info)) return(NULL)
   if(info$type == "group") {
       return(.Call("HDF_group_get_group", group, name, PACKAGE="rhdf5"))
   }
   if(info$type == "dataset") {
       return(.Call("HDF_group_get_dataset", group, name, PACKAGE="rhdf5"))
   }
   return(NULL)
}

"$.hdf5.group" <- function(group,name) "[[.hdf5.group"(group,name)

"[.hdf5.group" <- function(x,...) stop("not implemented")
#"[.hdf5.group" <- function(x,...) "[[.hdf5.group"(x,names(x)[..1])

#Why doesn't this work?
##comment out for now - not sure what containers were meant to do
#as.container <- function(x, ...) UseMethod("as.container")
#as.container.hdf5.group <- function(x, ...) {
#  #There should be some consistency checking here
#  class(x) <- c("container",class(x))
#  group
#}

hdf5.apply <- function(X, REGION, FUN=function(x) x,...) {
  FUN <- match.fun(FUN)
  .Call("HDF_group_apply", X, FUN, REGION, list(...), PACKAGE="rhdf5")
}
