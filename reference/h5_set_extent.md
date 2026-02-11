# Set a new dataset extension

Set a new dataset extension to an existing dataset in an HDF5 file

## Usage

``` r
h5set_extent(file, dataset, dims, native = FALSE)
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

- dataset:

  The name of the dataset in the HDF5 file, or an object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 dataset identifier. See
  [`H5Dcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dcreate.md),
  or
  [`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md)
  to create an object of this kind.

- dims:

  The dimensions of the array as they will appear in the file. Note, the
  dimensions will appear in inverted order when viewing the file with a
  C program (e.g. HDFView), because the fastest changing dimension in R
  is the first one, whereas the fastest changing dimension in C is the
  last one.

- native:

  An object of class `logical`. If `TRUE`, array-like objects are
  treated as stored in HDF5 row-major rather than R column-major
  orientation. Using `native = TRUE` increases HDF5 file portability
  between programming languages. A file written with `native = TRUE`
  should also be read with `native = TRUE`

## Value

Returns `TRUE` if the dimension of the dataset was changed successfully
and `FALSE` otherwise.

## Author

Bernd Fischer, Mike Smith

## Examples

``` r
tmpfile <- tempfile()
h5createFile(file = tmpfile)
h5createDataset(tmpfile, "A", c(10, 12), c(20, 24))
h5ls(tmpfile, all = TRUE)[c("dim", "maxdim")]
#>       dim  maxdim
#> 0 10 x 12 20 x 24
h5set_extent(tmpfile, "A", c(20, 24))
h5ls(tmpfile, all = TRUE)[c("dim", "maxdim")]
#>       dim  maxdim
#> 0 20 x 24 20 x 24
```
