# Write object to an HDF5 file.

Writes an R object to an HDF5 file. This function can be used to write
either full arrays/vectors or subarrays (hyperslabs) within an existing
dataset.

## Usage

``` r
h5write(obj, file, name, ...)

# Default S3 method
h5write(
  obj,
  file,
  name,
  createnewfile = TRUE,
  write.attributes = FALSE,
  ...,
  native = FALSE
)

h5writeDataset(obj, h5loc, name, ...)

# S3 method for class 'data.frame'
h5writeDataset(
  obj,
  h5loc,
  name,
  level = 6,
  chunk,
  DataFrameAsCompound = TRUE,
  ...
)

# S3 method for class 'array'
h5writeDataset(
  obj,
  h5loc,
  name,
  index = NULL,
  start = NULL,
  stride = NULL,
  block = NULL,
  count = NULL,
  size = NULL,
  variableLengthString = FALSE,
  encoding = NULL,
  level = 6,
  ...
)
```

## Arguments

- obj:

  The R object to be written.

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

- name:

  The name of the dataset in the HDF5 file.

- ...:

  Further arguments passed to
  [`H5Dwrite()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dwrite.md).

- createnewfile:

  If `TRUE`, a new file will be created if necessary.

- write.attributes:

  (logical) If TRUE, all R-attributes attached to the object `obj` are
  written to the HDF5 file.

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- level:

  The compression level. An integer value between 0 (no compression) and
  9 (highest and slowest compression). Only used, if the dataset does
  not yet exist. See
  [`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md)
  to create an dataset.

- chunk:

  Specifies the number of items to be include in an HDF5 chunk. If left
  unspecified the defaults is the smaller of: the total number of cols
  or the number of cols that fit within 4GB of memory. If
  `DataFrameAsCompound=FALSE` each row of the `data.frame` can be
  consider an "col".

- DataFrameAsCompound:

  If true, a `data.frame` will be saved as a compound data type.
  Otherwise it is saved like a list. The advantage of saving a
  data.frame as a compound data type is that it can be read as a table
  from python or with a struct-type from C. The disadvantage is that the
  data has to be rearranged on disk and thus can slow down I/O. If fast
  reading is required, `DataFrameAsCompound=FALSE` is recommended.

- index:

  List of indices for subsetting. The length of the list has to agree
  with the dimensional extension of the HDF5 array. Each list col is an
  integer vector of indices. A list col equal to `NULL` chooses all
  indices in this dimension. Counting is R-style 1-based.

- start:

  The start coordinate of a hyperslab (similar to subsetting in R).
  Counting is R-style 1-based. This argument is ignored, if index is not
  NULL.

- stride:

  The stride of the hypercube. Read the introduction
  [http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html](http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.md)
  before using this argument. R behaves like Fortran in this example.
  This argument is ignored, if index is not NULL.

- block:

  The block size of the hyperslab. Read the introduction
  [http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html](http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.md)
  before using this argument. R behaves like Fortran in this example.
  This argument is ignored, if index is not NULL.

- count:

  The number of blocks to be written. This argument is ignored, if index
  is not NULL.

- size:

  The length of the fixed-width string data type, when `obj` is a
  character vector. If `NULL`, this is set to the length of the largest
  string.

- variableLengthString:

  Whether character vectors should be written as variable-length strings
  into the attributes. If `TRUE`, `size` is ignored.

- encoding:

  The encoding of the string data type. Valid options are "ASCII" or
  "UTF-8".

## Value

`h5write` returns 0 if successful.

## Details

Writes an R object to an HDF5 file. If none of the arguments `start`,
`stride`, `block`, `count` is specified, the dataset has the same
dimension in the HDF5 file and in memory. If the dataset already exists
in the HDF5 file, one can write subarrays, (so called hyperslabs) to the
HDF5 file. The arguments `start`, `stride`, `block`, `count` define the
subset of the dataset in the HDF5 file that is to be written to. See
these introductions to hyperslabs:
<https://support.hdfgroup.org/HDF5/Tutor/selectsimple.html>,
<https://support.hdfgroup.org/HDF5/Tutor/select.html> and
[http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html](http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.md).
Please note that in R the first dimension is the fastest changing
dimension.

When viewing the HDF5 datasets with any C-program (e.g. HDFView), the
order of dimensions is inverted. In the R interface counting starts with
1, whereas in the C-programs (e.g. HDFView) counting starts with 0.

If code `obj` is of type 'complex' then it will be written as a compound
datatype to the HDF5, with cols named 'r' and 'i' for the real and
imaginary parts respectively.

## References

<https://portal.hdfgroup.org/display/HDF5>

## See also

[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md),
[`h5createFile()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createFile.md),
[`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md),
[rhdf5](https://huber-group-embl.github.io/rhdf5/reference/rhdf5.md)

## Author

Bernd Fischer, Mike Smith

## Examples

``` r

h5File <- tempfile(fileext = ".h5")
h5createFile(h5File)

# write a matrix
B <- array(seq(0.1, 2.0, by = 0.1), dim = c(5, 2, 2))
attr(B, "scale") <- "liter"
h5write(B, h5File, "B")

# write a submatrix
h5createDataset(h5File, "S", c(5, 8), storage.mode = "integer", chunk = c(5, 1), level = 7)
h5write(matrix(1:5, nr = 5, nc = 1), file = h5File, name = "S", index = list(NULL, 1))
```
