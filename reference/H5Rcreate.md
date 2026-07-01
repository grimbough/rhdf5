# Create a reference

Creates a reference to an object or dataset selection inside an HDF5
file.

## Usage

``` r
H5Rcreate(h5loc, name, ref_type = "H5R_OBJECT", h5space = NULL)
```

## Arguments

- h5loc:

  An `H5IdComponent` object representing the location to be pointed to
  by the created reference.

- name:

  Character string giving the name of the object to be referenced,
  relative to the location given by `h5loc`.

- ref_type:

  The type of reference to create. Accepts either `H5R_OBJECT` or
  `H5R_DATASET_REGION`.

- h5space:

  An object of class `H5IdComponent` representing a dataspace with a
  selection set. This argument is only used if creating a reference to a
  dataset region, and will be ignored otherwise.

## Value

An
[H5Ref](https://huber-group-embl.github.io/rhdf5/reference/H5Ref-class.md)
object storing the reference.

## Examples

``` r
## first we'll create a file with a group named "foo" and a
## 1-dimensional dataset named "baa" inside that group.
file_name <- tempfile(fileext = ".h5")
h5createFile(file_name)
h5createGroup(file = file_name, group = "/foo")
h5write(1:100, file = file_name, name = "/foo/baa")


fid <- H5Fopen(file_name)
ref_to_group <- H5Rcreate(fid, name = "/foo")
ref_to_dataset <- H5Rcreate(fid, name = "/foo/baa")
two_refs <- c(ref_to_group, ref_to_dataset)
two_refs
#> HDF5 REFERENCE
#> Type: H5R_OBJECT 
#> Length: 2 

## the size of this dataspace is the number of object references
## we want to store
sid <- H5Screate_simple(2)
tid <- H5Tcopy(dtype_id = "H5T_STD_REF_OBJ")
did <- H5Dcreate(fid, name = "object_refs", dtype_id = tid, h5space = sid)
H5Dwrite(did, two_refs)
H5Dclose(did)
H5Sclose(sid)
H5Fclose(fid)
```
