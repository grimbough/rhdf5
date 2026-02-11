# Create HDF5 group

Creates a group within an HDF5 file.

## Usage

``` r
h5createGroup(file, group)
```

## Arguments

- file:

  The filename (character) of the file in which the dataset will be
  located. For advanced programmers it is possible to provide an object
  of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- group:

  The name of the new group. The name can contain a hierarchy of
  groupnames, e.g. `"/group1/group2/newgroup"`, but the function will
  fail if the top level groups do not exists.

## Value

Returns TRUE is group was created successfully and FALSE otherwise.

## Details

Creates a new group within an HDF5 file.

## See also

[`h5createFile()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createFile.md),
[`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md),
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md),
[`h5write()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md)

## Author

Bernd Fischer

## Examples

``` r
h5File <- tempfile(pattern = "ex_createGroup.h5")
h5createFile(h5File)

# create groups
h5createGroup(h5File, "foo")
h5createGroup(h5File, "foo/foobaa")

h5ls(h5File)
#>   group   name     otype dclass dim
#> 0     /    foo H5I_GROUP           
#> 1  /foo foobaa H5I_GROUP           
```
