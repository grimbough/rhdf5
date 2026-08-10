# Create an HDF5 file

Create an HDF5 file

## Usage

``` r
H5Fcreate(
  name,
  flags = h5default("H5F_ACC"),
  fcpl = NULL,
  fapl = NULL,
  native = FALSE
)
```

## Arguments

- name:

  The name of the HDF5 file to create.

- flags:

  See `h5const("H5F_ACC")` for possible arguments.

- fcpl, fapl:

  Object object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md).
  This should representing a file creation property list and a file
  access property list respectively. See
  [`H5Pcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pcreate.md)
  or
  [`H5Pcopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pcopy.md)
  to create objects of this kind. Leaving as `NULL` will use the default
  HDF5 settings which are often sufficient.

- native:

  An object of class `logical`. If `TRUE`, array-like objects are
  treated as stored in HDF5 row-major rather than R column-major
  orientation. Using `native = TRUE` increases HDF5 file portability
  between programming languages. A file written with `native = TRUE`
  should also be read with `native = TRUE`.

## Examples

``` r
h5_file <- withr::local_tempfile(fileext = ".h5")
H5Fcreate(h5_file)

H5Fis_hdf5(h5_file)
#> [1] TRUE
```
