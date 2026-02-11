# Create HDF5 file

R function to create an empty HDF5 file.

## Usage

``` r
h5createFile(file)
```

## Arguments

- file:

  The filename of the HDF5 file.

## Value

Returns (invisibly) `TRUE` is file was created successfully and `FALSE`
otherwise.

## Details

Creates an empty HDF5 file.

## See also

[`h5createGroup()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createGroup.md),
[`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md),
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md),
[`h5write()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md),
[rhdf5](https://huber-group-embl.github.io/rhdf5/reference/rhdf5.md)

## Author

Bernd Fischer

## Examples

``` r
h5File <- tempfile(pattern = "ex_createFile.h5")

h5createFile(h5File)

# create groups
h5createGroup(h5File, "foo")
h5createGroup(h5File, "foo/foobaa")

h5ls(h5File)
#>   group   name     otype dclass dim
#> 0     /    foo H5I_GROUP           
#> 1  /foo foobaa H5I_GROUP           
```
