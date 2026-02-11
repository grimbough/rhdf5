# List the content of an HDF5 file.

List the content of an HDF5 file.

## Usage

``` r
h5ls(
  file,
  recursive = TRUE,
  all = FALSE,
  datasetinfo = TRUE,
  index_type = h5default("H5_INDEX"),
  order = h5default("H5_ITER"),
  s3 = FALSE,
  s3credentials = NULL,
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

- all:

  If `TRUE`, a longer list of information on each entry is provided.

- datasetinfo:

  If `FALSE`, datatype and dimensionality information is not provided.
  This can speed up the content listing for large files.

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

- native:

  An object of class `logical`. If TRUE, array-like objects are treated
  as stored in HDF5 row-major rather than R column-major orientation.
  Using `native = TRUE` increases HDF5 file portability between
  programming languages. A file written with `native = TRUE` should also
  be read with `native = TRUE`

## Value

`h5ls` returns a `data.frame` with the file content.

## References

<https://portal.hdfgroup.org/display/HDF5>

## See also

[`h5dump()`](https://huber-group-embl.github.io/rhdf5/reference/h5_dump.md)

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
h5ls(h5File, all = TRUE)
#>   group   name         ltype cset       otype num_attrs dclass          dtype
#> 0     /    foo H5L_TYPE_HARD    0   H5I_GROUP         0                      
#> 1  /foo      B H5L_TYPE_HARD    0 H5I_DATASET         1  FLOAT H5T_IEEE_F64LE
#> 2  /foo foobaa H5L_TYPE_HARD    0   H5I_GROUP         0                      
#>    stype rank       dim    maxdim
#> 0           0                    
#> 1 SIMPLE    3 5 x 2 x 2 5 x 2 x 2
#> 2           0                    

# list content of an hdf5 file in a public S3 bucket
# \donttest{
h5ls(file = "https://rhdf5-public.s3.eu-central-1.amazonaws.com/h5ex_t_array.h5", s3 = TRUE)
#>   group name       otype dclass dim
#> 0     /  DS1 H5I_DATASET  ARRAY   4
# }
```
