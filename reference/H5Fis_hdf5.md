# Determine whether a file is in the HDF5 format

`H5Fis_hdf5()` determines whether a file is in the HDF5 format.

## Usage

``` r
H5Fis_hdf5(name, showWarnings = TRUE)
```

## Arguments

- name:

  Character vector of length 1, giving the path to the file to be
  checked.

- showWarnings:

  If the file doesn't exist an warning is generated. Setting this
  argument to `FALSE` will suppress the warning.

## Value

Returns `TRUE`, if the file is an HDF5 file, or `FALSE` otherwise. In
the case the file doesn't exist, `NA` is returned

## Examples

``` r
h5_file <- withr::local_tempfile(fileext = ".h5")
H5Fcreate(h5_file)

H5Fis_hdf5(h5_file)
#> [1] TRUE
```
