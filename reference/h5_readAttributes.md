# Read all attributes from a given location in an HDF5 file

Read all attributes from a given location in an HDF5 file

## Usage

``` r
h5readAttributes(file, name, native = FALSE, ...)
```

## Arguments

- file:

  Character vector of length 1, giving the path to the HDF5

- name:

  Path within the HDF5 file to the object whose attributes should be
  read. The datasets present in `file` can be listed with the function
  [`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md).

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.

- ...:

  Further arguments passed to
  [`H5Aread()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aread.md).

## Value

A named list of the same length as the number of attributes attached to
the specific object. The names of the list entries correspond to the
attribute names. If no attributes are found an empty list is returned.
