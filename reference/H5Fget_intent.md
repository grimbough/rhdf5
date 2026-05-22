# Determine the read only or read/write status of an open file handle.

Determine the read only or read/write status of an open file handle.

## Usage

``` r
H5Fget_intent(h5file)
```

## Arguments

- h5file:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 file identifier. Typically produced by
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
  or
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md).

## Value

Returns a character vector of length 1. This will either be
`H5F_ACC_RDWR` (read / write) or `H5F_ACC_READONLY` (read only).

## Details

The native `H5Fget_intent()` function can in theory also return the
values `H5F_ACC_SWMR_WRITE` and `H5F_ACC_SWMR_READ`. However these
require the underlying HDF5 library to be complied with support for
single-writer/multiple-reader (SWMR), which Rhdf5lib currently is not.
Hence only the two values detailed in the values section should be
possible.

## Examples

``` r

## use an example file and show its location
h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
## open the file as read only and check this
fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
H5Fget_intent(fid)
#> [1] "H5F_ACC_RDONLY"
H5Fclose(fid)

## open file as read write and confirm
fid <- H5Fopen(h5file, flags = "H5F_ACC_RDWR")
H5Fget_intent(fid)
#> [1] "H5F_ACC_RDWR"
H5Fclose(fid)
```
