# IO related functions

hdf5.readcel <- function(set, file, create = hdf5.default.properties)
    .Call("HDF_readCEL", set, file, create)
