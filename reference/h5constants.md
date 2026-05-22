# HDF5 library constants.

Access to HDF5 constants.

## Usage

``` r
h5const(type = "")

h5constType()

h5default(type = "")
```

## Arguments

- type:

  A character name of a group of constants.

## Value

A character vector with names of HDF5 constants or groups.

## Details

These functions provide a list of HDF5 constants that are defined in the
R package. `h5constType` provides a list of group names and `h5const`
gives the constants defined within a group. `h5default` gives the
default choice for each group.

## Author

Bernd Fischer

## Examples

``` r

h5constType()[1]
#> [1] "H5F_ACC"
h5const(h5constType()[1])
#> [1] "H5F_ACC_TRUNC" "H5F_ACC_EXCL" 
```
