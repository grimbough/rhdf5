# Find the size of an open HDF5 file

`H5Fget_filesize()` returns the size in bytes of the HDF5 file specified
by `h5file`.

## Usage

``` r
H5Fget_filesize(h5file)
```

## Arguments

- h5file:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an HDF5 file ID. Typically created via
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md)
  or
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md).

## Examples

``` r
h5_file <- withr::local_tempfile(fileext = ".h5")
H5Fcreate(h5_file)

fid <- H5Fopen(h5_file)
H5Fget_filesize(fid)
#> [1] 2048
H5Fclose(fid)
```
