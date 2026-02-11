# Move a link within an HDF5 file

Move a link within an HDF5 file

## Usage

``` r
H5Lmove(h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group) where the new
  link is placed.

- name:

  The name of the link to be moved.

- h5loc_dest:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the H5 location where the new link should be
  created.

- name_dest:

  Name of the new link to be created

- lcpl, lapl:

  Link creation and link access property lists to be associated with the
  new link. Leaving these arguments as `NULL` will use the HDF5 default
  property lists.

## Examples

``` r
## create an HDF5 file with a single group
## that contains a dataset of 10 numbers
h5file <- tempfile(fileext = ".h5")
h5createFile(h5file)
h5createGroup(h5file, "/foo")
h5write(1:10, h5file, name = "/foo/vector1")
## check the structure is what we expect
h5ls(h5file)
#>   group    name       otype  dclass dim
#> 0     /     foo   H5I_GROUP            
#> 1  /foo vector1 H5I_DATASET INTEGER  10

## open the file, the group where the dataset currently is
## and the root group
fid <- H5Fopen(name = h5file)
gid1 <- H5Gopen(fid, "/foo")
gid2 <- H5Gopen(fid, "/")
## move the dataset to the root of the file and rename it
H5Lmove(gid1, "vector1", gid2, "vector_new")
h5closeAll()
## check the dataset has moved out of the foo group
h5ls(h5file)
#>   group       name       otype  dclass dim
#> 0     /        foo   H5I_GROUP            
#> 1     / vector_new H5I_DATASET INTEGER  10

## we can also provide the ID of the HDF5 file
## and use the "name" arguments to move between groups
fid <- H5Fopen(name = h5file)
H5Lmove(fid, "/vector_new", fid, "/foo/vector_newer")
H5Fclose(fid)
h5ls(h5file)
#>   group         name       otype  dclass dim
#> 0     /          foo   H5I_GROUP            
#> 1  /foo vector_newer H5I_DATASET INTEGER  10
```
