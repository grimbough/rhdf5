# Dump the content of an HDF5 file.

Dump the content of an HDF5 file.

## Usage

``` r
h5dump(
  file,
  recursive = TRUE,
  load = TRUE,
  all = FALSE,
  index_type = h5default("H5_INDEX"),
  order = h5default("H5_ITER"),
  s3 = FALSE,
  s3credentials = NULL,
  ...,
  native = FALSE
)
```

## Arguments

- file:

  The filename (character) of the file in which the dataset will be
  located. You can also provide an object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- recursive:

  If `TRUE`, the content of the whole group hierarchy is listed. If
  `FALSE`, Only the content of the main group is shown. If a positive
  integer is provided this indicates the maximum level of the hierarchy
  that is shown.

- load:

  If `TRUE` the datasets are read in, not only the header information.
  Note, that this can cause memory problems for very large files. In
  this case choose `load=FALSE` and load the datasets successively.

- all:

  If `TRUE`, a longer list of information on each entry is provided.

- index_type:

  See `h5const("H5_INDEX")` for possible arguments.

- order:

  See `h5const("H5_ITER")` for possible arguments.

- s3:

  Logical value indicating whether the file argument should be treated
  as a URL to an Amazon S3 bucket, rather than a local file path.

- s3credentials:

  A list of length three, providing the credentials for accessing files
  in a private Amazon S3 bucket.

- ...:

  Arguments passed to
  [`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md)

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

## Value

Returns a hierarchical list structure representing the HDF5 group
hierarchy. It either returns the datasets within the list structure
(`load=TRUE`) or it returns a `data.frame` for each dataset with the
dataset header information (`load=FALSE`).

## See also

[`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md)

## Author

Bernd Fischer, Mike L. Smith

## Examples

``` r

h5File <- tempfile(pattern = "ex_dump.h5")
h5createFile(h5File)

# create groups
h5createGroup(h5File, "foo")
h5createGroup(h5File, "foo/foobaa")

# write a matrix
B <- array(seq(0.1, 2.0, by = 0.1), dim = c(5, 2, 2))
attr(B, "scale") <- "liter"
h5write(B, h5File, "foo/B")

# list content of hdf5 file
h5dump(h5File)
#> $foo
#> $foo$B
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]  0.1  0.6
#> [2,]  0.2  0.7
#> [3,]  0.3  0.8
#> [4,]  0.4  0.9
#> [5,]  0.5  1.0
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]  1.1  1.6
#> [2,]  1.2  1.7
#> [3,]  1.3  1.8
#> [4,]  1.4  1.9
#> [5,]  1.5  2.0
#> 
#> 
#> $foo$foobaa
#> NULL
#> 
#> 

# list content of an hdf5 file in a public S3 bucket
# \donttest{
h5dump(file = "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5", s3 = TRUE)
#> $DS1
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    0   -1   -2
#> [3,]    0   -2   -4
#> [4,]    0   -3   -6
#> [5,]    0   -4   -8
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    1    2
#> [2,]    1    1    1
#> [3,]    2    1    0
#> [4,]    3    1   -1
#> [5,]    4    1   -2
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    2    4
#> [2,]    2    3    4
#> [3,]    4    4    4
#> [4,]    6    5    4
#> [5,]    8    6    4
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    3    6
#> [2,]    3    5    7
#> [3,]    6    7    8
#> [4,]    9    9    9
#> [5,]   12   11   10
#> 
#> 
# }
```
