# An S4 class representing an H5 object

A class representing a HDF5 identifier handle. HDF5 identifiers
represent open files, groups, datasets, dataspaces, attributes, and
datatypes.

## Usage

``` r
# S4 method for class 'H5IdComponent'
show(object)

# S4 method for class 'H5IdComponent,character'
e1 & e2

# S4 method for class 'H5IdComponent'
x$name

# S4 method for class 'H5IdComponent'
x$name <- value

# S4 method for class 'H5IdComponent'
x[i, j, ..., drop = TRUE]

# S4 method for class 'H5IdComponent'
x[i, j, ...] <- value
```

## Arguments

- object:

  Object of class `H5IdComponent`

- e1:

  An `H5IdComponent` object representing an H5 file or group.

- e2:

  Character giving the path to an HDF5 group or dataset relative to
  `e1`.

- x:

  Object of class `H5IdComponent` representing the HDF5 dataset from
  which to extract element(s) or in which to replace element(s).

- name:

  Character giving the path to an HDF5 group or dataset relative to `x`.

- value:

  Array-like R object containing value to be inserted into the HDF5
  dataset.

- i, j, ...:

  Indices specifying elements to extract or replace. Indices are
  `numeric` vectors or empty (missing) or `NULL`. Numeric values are
  coerced to integer as by
  [`base::as.integer()`](https://rdrr.io/r/base/integer.html) (and hence
  truncated towards zero).

- drop:

  If `TRUE` the result is coerced to the lowest possible dimension (see
  the examples). This only works for extracting elements, not for the
  replacement. See [`base::drop()`](https://rdrr.io/r/base/drop.html)
  for further details.

## Methods (by generic)

- `show(H5IdComponent)`: Print details of the object to screen.

- `e1 & e2`: Returns a group handle or dataset handle for the group or
  dataset `name` in the HDF5 location `h5loc`. `h5loc` can either be a
  file handle as returned by
  [H5Fopen](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
  or a group handle as e.g. returned by `h5f$g1` or `h5f$'/g1/g2'`.

- `$`: Reads the HDF5 object `name` in the HDF5 location `x`. `x` can
  either be a file handle as returned by
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
  or a group handle as e.g. returned by `h5f$g1` or `h5f$'/g1/g2'`.

- `` `$`(H5IdComponent) <- value ``: Writes the assigned object to to
  the HDF5 file at location e1. e1 can either be a file handle as
  returned by
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
  or a group handle as e.g. returned by h5f\$g1 or h5f\$'/g1/g2's. The
  storage.mode of the assigned object has to be compatible to the
  datatype of the HDF5 dataset. The dimension of the assigned object
  have to be identical the dimensions of the HDF5 dataset. To create a
  new HDF5 dataset with specific properties (e.g. compression level or
  chunk size), please use the function
  [`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md)
  first.

- `[`: Subsetting of an HDF5 dataset. The function reads a subset of an
  HDF5 dataset. The given dimensions have to fit the dimensions of the
  HDF5 dataset.

- `` `[`(H5IdComponent) <- value ``: Subsetting of an HDF5 dataset. The
  function writes an R data object to a subset of an HDF5 dataset. The
  given dimensions have to fit the dimensions of the HDF5 dataset. The
  HDF5 dataset has to be created beforehand, e.g. by
  [`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md).

## Slots

- `ID`:

  `integer` of length 1. Contains the handle of C-type `hid_t`.

- `native`:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`
