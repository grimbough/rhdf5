#
# HDF5 File Related Functions
#

hdf5.open <- function(filename, access=hdf5.default.properties)
  .Call("HDF_file_open", filename, access, PACKAGE="rhdf5")

hdf5.create <- function(filename, mode="e",
  create=hdf5.default.properties, access=hdf5.default.properties)
  .Call("HDF_file_create", filename, mode, create, access, PACKAGE="rhdf5")

#These functions take group-like behavior
names.hdf5.file <- function(x, ...)      names.hdf5.group(x)
print.hdf5.file <- function(x, ...)      print.hdf5.group(x)
"$.hdf5.file"   <- function(file, name) "$.hdf5.group"(file, name)
"[[.hdf5.file"  <- function(file, name) "[[.hdf5.group"(file,name)
"[.hdf5.file" <- function(x, i, j, drop)
    stop("[ is not implemented for hdf5.file")


