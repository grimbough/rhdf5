#
# HDF5 Property List Functions
#
hdf5.plist.create <- function(type,...) {
  switch(type,
         file.create=return(.Call("HDF_plist_file_create",..1,..2)),
         file.access=return(.Call("HDF_plist_file_access")),
         dataset.create=return(.Call("HDF_plist_dataset_create")),
         dataset.xfer=return(.Call("HDF_plist_dataset_xfer")),
         mount=return(.Call("HDF_plist_mount")))
  return(hdf5.default.properties)
}

hdf5.plist.set.cache <- function(plist,bytes)
  .Call("HDF_plist_set_cache",plist,bytes)

print.hdf5.proplist <- function(x, ...)
  .Call("HDF_plist_print", x)

hdf5.get.default.plist <- function() .Call("HDF_plist_default_plist")

.initHDF5 <- function(where) {
    assign("hdf5.default.properties", hdf5.get.default.plist(),
    env=where)
    assign(".hdf5.Rwork", .hdf5.Rwork, env=where)
    assign(".hdf5.Rwork.Current", .hdf5.Rwork.Current, env=where)
    assign(".hdf5.space.dims.symbol" , .hdf5.space.dims.symbol, env=where)
}
