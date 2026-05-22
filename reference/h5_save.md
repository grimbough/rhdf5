# Saves a one or more objects to an HDF5 file.

Saves a number of R objects to an HDF5 file.

## Usage

``` r
h5save(..., file, name = NULL, createnewfile = TRUE, native = FALSE)
```

## Arguments

- ...:

  The objects to be saved.

- file:

  The filename (character) of the file in which the dataset will be
  located. It is also possible to provide an object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- name:

  A character vector of names for the datasets. The length of the name
  vector should match the number of objects.

- createnewfile:

  If `TRUE`, a new file will be created if necessary.

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

## Value

Nothing returned.

## Details

The objects will be saved to the HDF5 file. If the file does not exists
it will be created. The data can be read again by either
[`h5dump()`](https://huber-group-embl.github.io/rhdf5/reference/h5_dump.md)
or individually for each dataset by
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md).

## See also

[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md),
[`h5write()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md)

## Author

Bernd Fischer

## Examples

``` r

A <- 1:7
B <- 1:18
D <- seq(0, 1, by = 0.1)

h5File <- tempfile(pattern = "ex_save.h5")
h5save(A, B, D, file = h5File)
h5dump(h5File)
#> $A
#> [1] 1 2 3 4 5 6 7
#> 
#> $B
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
#> 
#> $D
#>  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
#> 
```
