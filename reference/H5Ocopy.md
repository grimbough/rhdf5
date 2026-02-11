# Copies an HDF5 object

Copies an HDF5 object

## Usage

``` r
H5Ocopy(h5loc, name, h5loc_dest, name_dest, obj_cpy_pl = NULL, lcpl = NULL)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 object where the source object should be
  copied from.

- name:

  Character vector of length 1, giving the name of the source object to
  be copied.

- h5loc_dest:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 object where the new copy should be created.

- name_dest:

  Character vector of length 1, giving the name of the new object to be
  created.

- obj_cpy_pl, lcpl:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  objects representing object copy and link creation property lists
  respectively. If left as `NULL` the default values for these will be
  used.

## Examples

``` r
## Create a temporary copy of an example file check the contents
example_file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
file.copy(example_file, tempdir())
#> [1] TRUE
h5_file <- file.path(tempdir(), "h5ex_t_array.h5")
h5ls(h5_file)
#>   group name       otype dclass dim
#> 0     /  DS1 H5I_DATASET  ARRAY   4

## open the example file and create a new, empty, file
fid1 <- H5Fopen(h5_file)
h5_file2 <- tempfile(fileext = ".h5")
fid2 <- H5Fcreate(h5_file2)

## We can copy a dataset inside the same file
H5Ocopy(h5loc = fid1, name = "DS1", h5loc_dest = fid1, name_dest = "DS2")
## Or to a different file
H5Ocopy(h5loc = fid1, name = "DS1", h5loc_dest = fid2, name_dest = "DS1_copy")

## if we want to create a new group hierarchy we can use a link creation property list
lcpl <- H5Pcreate("H5P_LINK_CREATE")
H5Pset_create_intermediate_group(lcpl, create_groups = TRUE)
H5Ocopy(
  h5loc = fid1, name = "DS1", h5loc_dest = fid2,
  name_dest = "/foo/baa/DS1_nested", lcpl = lcpl
)

## tidy up
H5Pclose(lcpl)
H5Fclose(fid1)
H5Fclose(fid2)

## Check we now have groups DS1 and DS2 in the original file
h5ls(h5_file)
#>   group name       otype dclass dim
#> 0     /  DS1 H5I_DATASET  ARRAY   4
#> 1     /  DS2 H5I_DATASET  ARRAY   4
## Check we have a copy of DS1 at the root and nests in the new file
h5ls(h5_file2)
#>      group       name       otype dclass dim
#> 0        /   DS1_copy H5I_DATASET  ARRAY   4
#> 1        /        foo   H5I_GROUP           
#> 2     /foo        baa   H5I_GROUP           
#> 3 /foo/baa DS1_nested H5I_DATASET  ARRAY   4
```
