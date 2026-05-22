# List all open HDF5 objects.

A list of all valid HDF5 identifiers. H5 objects should be closed after
usage to release resources.

## Usage

``` r
h5listIdentifier()

h5validObjects(native = FALSE)
```

## Arguments

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

## Value

`h5validObjects` returns a list of
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
objects. `h5listIdentifier` prints the valid identifiers on screen and
returns NULL.

## Author

Bernd Fischer, Mike Smith

## Examples

``` r

h5File <- tempfile("ex_list_identifier.h5")

h5createFile(h5File)

# create groups
h5createGroup(h5File, "foo")

h5listIdentifier()
#> [1] type name
#> <0 rows> (or 0-length row.names)
h5validObjects()
#> list()
```
