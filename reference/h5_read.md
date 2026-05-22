# Reads and write object in HDF5 files

Reads objects in HDF5 files. This function can be used to read either
full arrays/vectors or subarrays (hyperslabs) from an existing dataset.

## Usage

``` r
h5read(
  file,
  name,
  index = NULL,
  start = NULL,
  stride = NULL,
  block = NULL,
  count = NULL,
  compoundAsDataFrame = TRUE,
  callGeneric = TRUE,
  read.attributes = FALSE,
  drop = FALSE,
  ...,
  native = FALSE,
  s3 = FALSE,
  s3credentials = NULL
)
```

## Arguments

- file:

  The file name (character) of the file in which the dataset is be
  located. It is possible to provide an object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- name:

  The name of the dataset in the HDF5 file. The datasets present in
  `file` can be listed with the function
  [`h5ls`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md).

- index:

  List of indices for subsetting. The length of the list has to agree
  with the dimensional extension of the HDF5 array. Each list element is
  an integer vector of indices. A list element equal to NULL chooses all
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

  The number of blocks to be read. This argument is ignored, if index is
  not NULL.

- compoundAsDataFrame:

  If true, a compound datatype will be coerced to a data.frame. This is
  not possible, if the dataset is multi-dimensional. Otherwise the
  compound datatype will be returned as a list. Nested compound data
  types will be returned as a nested list.

- callGeneric:

  If TRUE a generic function h5read.classname will be called if it
  exists depending on the dataset's class attribute within the HDF5
  file. This function can be used to convert the standard output of
  h5read depending on the class attribute. Note that h5read is not a S3
  generic function. Dispatching is done based on the HDF5 attribute
  after the standard h5read function.

- read.attributes:

  (logical) If `TRUE`, the HDF5 attributes are read and attached to the
  respective R object.

- drop:

  (logical) If TRUE, the HDF5 object is read as a vector with `NULL` dim
  attributes.

- ...:

  Further arguments passed to
  [`H5Dread()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dread.md).

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

- s3:

  Logical value indicating whether the file argument should be treated
  as a URL to an Amazon S3 bucket, rather than a local file path.

- s3credentials:

  A list of length three, providing the credentials for accessing files
  in a private Amazon S3 bucket.

## Value

`h5read` returns an array with the data read.

## Details

Read an R object from an HDF5 file. If none of the arguments `start`,
`stride`, `block`, `count` are specified, the dataset has the same
dimension in the HDF5 file and in memory. If the dataset already exists
in the HDF5 file, one can read subarrays, so called hyperslabs from the
HDF5 file. The arguments `start`, `stride`, `block`, `count` define the
subset of the dataset in the HDF5 file that is to be read/written. See
these introductions to hyperslabs:
<https://support.hdfgroup.org/HDF5/Tutor/selectsimple.html>,
<https://support.hdfgroup.org/HDF5/Tutor/select.html> and
[http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.html](http://ftp.hdfgroup.org/HDF5/Tutor/phypecont.md).
Please note that in R the first dimension is the fastest changing
dimension.

When viewing the HDF5 datasets with any C-program (e.g. HDFView), the
order of dimensions is inverted. In the R interface counting starts with
1, whereas in the C-programs (e.g. HDFView) counting starts with 0.

Special cases. There are a few instances where rhdf5 will make
assumptions about the dataset you are reading and treat it slightly
differently. 1) complex numbers. If your datasets is a compound
datatype, has only two columns, and these are named 'r' and 'i' rhdf5
will assume the data is intended to be complex numbers and will read
this into R's complex type. If that is not the case, you will need to
extract the two values separately using the
[`Re()`](https://rdrr.io/r/base/complex.html) and
[`Im()`](https://rdrr.io/r/base/complex.html) accessors manually.

## See also

[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md)

## Author

Bernd Fischer, Mike Smith

## Examples

``` r

h5File <- tempfile(pattern = "ex_hdf5file.h5")
h5createFile(h5File)

# write a matrix
B <- array(seq(0.1, 2.0, by = 0.1), dim = c(5, 2, 2))
h5write(B, h5File, "B")

# read a matrix
E <- h5read(h5File, "B")

# write and read submatrix
h5createDataset(h5File, "S", c(5, 8), storage.mode = "integer", chunk = c(5, 1), level = 7)
h5write(matrix(1:5, nr = 5, nc = 1), file = h5File, name = "S", index = list(NULL, 1))
h5read(h5File, "S")
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]    1    0    0    0    0    0    0    0
#> [2,]    2    0    0    0    0    0    0    0
#> [3,]    3    0    0    0    0    0    0    0
#> [4,]    4    0    0    0    0    0    0    0
#> [5,]    5    0    0    0    0    0    0    0
h5read(h5File, "S", index = list(NULL, 2:3))
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    0
#> [3,]    0    0
#> [4,]    0    0
#> [5,]    0    0

# Read a subset of an hdf5 file in a public S3 bucket
# \donttest{
h5read("https://rhdf5-public.s3.eu-central-1.amazonaws.com/rhdf5ex_t_float_3d.h5",
  s3 = TRUE, name = "a1", index = list(NULL, 3, NULL)
)
#> , , 1
#> 
#>           [,1]
#> [1,] 0.2444485
#> [2,] 0.3873723
#> [3,] 0.4045331
#> [4,] 0.6178528
#> [5,] 0.5530508
#> 
#> , , 2
#> 
#>           [,1]
#> [1,] 0.7906603
#> [2,] 0.3274960
#> [3,] 0.2159982
#> [4,] 0.8083879
#> [5,] 0.7067179
#> 
# }
```
