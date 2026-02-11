# Create HDF5 attribute

R function to create an HDF5 attribute and defining its dimensionality.

## Usage

``` r
h5createAttribute(
  obj,
  attr,
  dims,
  maxdims = dims,
  file,
  storage.mode = "double",
  H5type = NULL,
  size = NULL,
  encoding = NULL,
  native = FALSE
)
```

## Arguments

- obj:

  The name (character) of the object the attribute will be attached to.
  For advanced programmers it is possible to provide an object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 object identifier (file, group, dataset). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md),
  [`H5Dcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dcreate.md),
  [`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md)
  to create an object of this kind.

- attr:

  Name of the attribute to be created.

- dims:

  The dimensions of the attribute as a numeric vector. If `NULL`, a
  scalar dataspace will be created instead.

- maxdims:

  The maximum extension of the attribute.

- file:

  The filename (character) of the file in which the dataset will be
  located. For advanced programmers it is possible to provide an object
  of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an H5 location identifier. See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind. The `file` argument is not required,
  if the argument `obj` is of type `H5IdComponent`.

- storage.mode:

  The storage mode of the data to be written. Can be obtained by
  `storage.mode(mydata)`.

- H5type:

  Advanced programmers can specify the datatype of the dataset within
  the file, either as a string with one of the available datatypes
  listed in `h5const("H5T")`, or as the output of
  [`H5Tcopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Tcopy.md).
  If `H5type` is specified the argument `storage.mode` is ignored. It is
  recommended to use `storage.mode`.

- size:

  The maximum string length when `storage.mode='character'`. If this is
  specified, HDF5 stores each string of `attr` as fixed length character
  arrays. Together with compression, this should be efficient.

  If this argument is set to `NULL`, HDF5 will instead store
  variable-length strings.

- encoding:

  The encoding of the string data type. Valid options are "ASCII" or
  "UTF-8".

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

## Value

Returns TRUE is attribute was created successfully and FALSE otherwise.

## Details

Creates a new attribute and attaches it to an existing HDF5 object. The
function will fail, if the file doesn't exist or if there exists already
another attribute with the same name for this object.

You can use
[`h5writeAttribute()`](https://huber-group-embl.github.io/rhdf5/reference/h5_writeAttribute.md)
immediately. It will create the attribute for you.

## References

<https://portal.hdfgroup.org/display/HDF5>

## See also

[`h5createFile()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createFile.md),
[`h5createGroup()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createGroup.md),
[`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md),
[`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md),
[`h5write()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md),
[rhdf5](https://huber-group-embl.github.io/rhdf5/reference/rhdf5.md)

## Author

Bernd Fischer

## Examples

``` r
h5File <- tempfile(pattern = "ex_createAttribute.h5")
h5createFile(h5File)
h5write(1:1, h5File, "A")
fid <- H5Fopen(h5File)
did <- H5Dopen(fid, "A")
h5createAttribute(did, "time", c(1, 10))
#> [1] TRUE
H5Dclose(did)
H5Fclose(fid)
```
