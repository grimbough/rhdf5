#
# HDF5 Dataspace Functions
#

#obtain a dataspace

space <- function(set, ...) UseMethod("space")
space.hdf5.dataset <- function(set, ...)
    .Call("HDF_dataset_getspace", set, PACKAGE="rhdf5")


print.hdf5.dataspace <- function(x, ...)
  .Call("HDF_space_print", x, PACKAGE="rhdf5")

hdf5.select.hyperslab <- function(space, selop = "SET", start, stride,
                                  count, block)
{
    sd <- dims(space)
    nd <- length(sd)
    if(missing(start) )
        start <- rep(1, nd)
    if(missing(block) )
        block <- rep(1, nd)
    if(missing(stride) )
        stride <- rep(1, nd)
    if(missing(count) )
        count <- rep(1, nd)

    .Call("HDF_select_hyperslab", space, selop, start, stride, count,
          block, PACKAGE="rhdf5")
}
