####Functions to manipulate HDF5 attributes

"hdf5.attr<-" <- function(set, name, value) {
  attribute(set,name) <- value;         #All HDF5 attributes are replicated in the R space
  invisible(.Call("HDF_attr_set", set, name, value, PACKAGE="rhdf5"))
}
hdf5.attr <- function(set, name)
	.Call("HDF_attr_get", set, name, PACKAGE="rhdf5")
hdf5.attributes <- function(set)
	.Call("HDF_attribute_get", set, PACKAGE="rhdf5")

