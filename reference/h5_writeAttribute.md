# Write an R object as an HDF5 attribute

Write an R object as an HDF5 attribute

## Usage

``` r
h5writeAttribute(
  attr,
  h5obj,
  name,
  h5loc,
  encoding = NULL,
  variableLengthString = TRUE,
  asScalar = FALSE,
  checkForNA = TRUE
)

# S3 method for class 'array'
h5writeAttribute(
  attr,
  h5obj,
  name,
  h5loc,
  encoding = NULL,
  variableLengthString = TRUE,
  asScalar = FALSE,
  checkForNA = TRUE
)
```

## Arguments

- attr:

  The R object to be written as an HDF5 attribute.

- h5obj:

  Normally an object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 object identifier (file, group, or dataset). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md),
  [`H5Dcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dcreate.md),
  or
  [`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md)
  to create an object of this kind. This argument can also be given the
  path to an HDF5 file.

- name:

  The name of the attribute to be written.

- h5loc:

  The location of the group or dataset within a file to which the
  attribute should be attached. This argument is only used if the
  `h5obj` argument is the path to an HDF5 file, otherwise it is ignored.

- encoding:

  The encoding of the string data type. Valid options are "ASCII" and
  "UTF-8".

- variableLengthString:

  Whether character vectors should be written as variable-length strings
  into the attributes.

- asScalar:

  Whether length-1 `attr` should be written into a scalar dataspace.

- checkForNA:

  Whether a `attr` should be checked for `NA` values before being
  written. This only applies of `attr` is of type logical. Testing for
  `NA` values can be slow if the object to be written is large, so if
  you are sure no such values will be present this argument can be used
  to disable the testing.

## Examples

``` r
hdf5_file <- tempfile()
h5createFile(hdf5_file)
h5createGroup(hdf5_file, "group")

values_to_be_written <- c(NA, FALSE, TRUE, FALSE, NA)
h5writeAttribute(
  values_to_be_written,
  h5obj = hdf5_file,
  name = "test",
  h5loc = "/group"
)

h5readAttributes(hdf5_file, name = "/group")
#> $test
#> [1]    NA FALSE  TRUE FALSE    NA
#> 
```
