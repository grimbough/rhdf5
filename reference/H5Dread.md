# Read from an HDF5 dataset

`H5Dread()` reads a (partial) dataset from an HDF5 file into the R
session.

## Usage

``` r
H5Dread(
  h5dataset,
  h5spaceFile = NULL,
  h5spaceMem = NULL,
  buf = NULL,
  compoundAsDataFrame = TRUE,
  bit64conversion = c("int", "double", "bit64"),
  drop = FALSE
)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset.

- h5spaceFile:

  An object of class `H5IdComponent` representing a HDF5 dataspace. See
  [`H5Dget_space()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_space.md),
  [`H5Screate_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate_simple.md),
  [`H5Screate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate.md)
  to create an object of this kind.

- h5spaceMem:

  An object of class `H5IdComponent` representing a HDF5 dataspace. See
  [`H5Dget_space()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_space.md),
  [`H5Screate_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate_simple.md),
  [`H5Screate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate.md)
  to create an object of this kind. The dimensions of the dataset in the
  file and in memory. The dimensions in file and in memory are
  interpreted in an R-like manner. The first dimension is the fastest
  changing dimension. When reading the file with a C-program (e.g.
  HDFView) the order of dimensions will invert, because in C the fastest
  changing dimension is the last one.

- buf:

  Buffer to hold the read data. The buffer size has to fit the size of
  the memory space `h5spaceMem`. No extra memory will be allocated for
  the data. A pointer to the same data is returned.

- compoundAsDataFrame:

  Logical vector of length 1. If `TRUE`, a compound datatype will be
  coerced to a `data.frame`. This is not possible, if the dataset is
  multi-dimensional. Otherwise the compound datatype will be returned as
  a `list`. Nested compound data types will be returned as a nested
  `list`.

- bit64conversion:

  Defines how 64-bit integers are converted. (See the details section
  for more information on these options.)

- drop:

  Logical vector of length 1. If `TRUE`, the HDF5 object is read as a
  vector with NULL dim attributes. Default is `FALSE`.

## Details

Internally, R does not support 64-bit integers. All integers in R are
32-bit integers. By setting bit64conversion='int', a coercing to 32-bit
integers is enforced, with the risk of data loss, but with the insurance
that numbers are represented as integers. bit64conversion='double'
coerces the 64-bit integers to floating point numbers. doubles can
represent integers with up to 54-bits, but they are not represented as
integer values anymore. For larger numbers there is again a data loss.
bit64conversion='bit64' is recommended way of coercing. It represents
the 64-bit integers as objects of class 'integer64' as defined in the
package 'bit64'. Make sure that you have installed 'bit64'. The datatype
'integer64' is not part of base R, but defined in an external package.
This can produce unexpected behaviour when working with the data.
