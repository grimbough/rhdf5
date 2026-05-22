# Test and set file locking for HDF5

HDF5 uses file locking by default. On some file systems this is not
available, and the HDF5 library will throw an error if the user attempts
to create or access a file located on such a file system. These
functions help identify if file locking is available without throwing an
error, and allow the locking to be disabled for the duration of the R
session if needed.

## Usage

``` r
h5testFileLocking(location)

h5disableFileLocking()

h5enableFileLocking()
```

## Arguments

- location:

  The name of a directory or file to test. If an existing directory is
  provided a temporary file will be created in this folder. If
  non-existent location is provided a file with the name will be
  created, tested for file locking, and then removed. Providing an
  existing file will result in an error.

## Value

`h5testFileLocking` returns `TRUE` if a file can be successfully locked
at the specified location, or `FALSE` otherwise.

`h5disableFileLocking` and `h5enableFileLocking` set are called for the
side effect of setting or unsetting the environment variable
`HDF5_USE_FILE_LOCKING` and do not return anything.

## Details

`h5testFileLocking` will create a temporary file and then attempt to
apply a file lock using the appropriate function within the HDF5
library. The success or failure of the locking is then recorded and the
temporary file removed. Even relatively low level functions such as
[`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md)
will fail inelegantly if file locking fails.

`h5disableFileLocking` will set the environment variable
`HDF5_USE_FILE_LOCKING=FALSE`, which is the recommended was to disable
this behaviour if file locking is not supported. This will only persist
within the current R session. You can set the environment variable
outside of R if this is a more general issue on your system.

`h5enableFileLocking` will unset the `HDF5_USE_FILE_LOCKING` environment
variable.

More discussion of HDF5's use of file locking can be found online e.g.
<https://forum.hdfgroup.org/t/hdf5-1-10-0-and-flock/3761/4> or
<https://forum.hdfgroup.org/t/hdf5-files-on-nfs/3985/5>

## Author

Mike Smith

## Examples

``` r

## either a file name or directory can be tested
file <- tempfile()
dir <- tempdir()

h5testFileLocking(dir)
#> [1] TRUE
h5testFileLocking(file)
#> [1] TRUE

## we can check for file locking, and disable if needed
if (!h5testFileLocking(dir)) {
  h5disableFileLocking()
}
```
