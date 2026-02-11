# Read the time stamps associated with an HDF5 group or dataset.

Read the time stamps associated with an HDF5 group or dataset.

## Usage

``` r
h5readTimestamps(file, name)
```

## Arguments

- file:

  Character vector of length 1, giving the path to the HDF5 file

- name:

  Path within the HDF5 file to the object whose attributes should be
  read. The datasets present in `file` can be listed with the function
  [`h5ls`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md).

## Value

A named list of length 4 containing the timestamps on the object. The
timestamps themselves are `POSIXct` objects (see
[`base::DateTimeClasses()`](https://rdrr.io/r/base/DateTimeClasses.html)).

## Details

All timestamps are returned in the UTC timezone. HDF5 objects can have
between 0 and 4 timestamps set, depending on the property lists provided
when they are created or accessed. Timestamps that are not tracked will
be returned as the UNIX epoch `1970-01-01 UTC`.

## Examples

``` r
# example file
example_file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")

## read timestamps on a group
h5readTimestamps(example_file, name = "/")
#> $access_time
#> [1] "1970-01-01 UTC"
#> 
#> $modification_time
#> [1] "1970-01-01 UTC"
#> 
#> $change_time
#> [1] "1970-01-01 UTC"
#> 
#> $birth_time
#> [1] "1970-01-01 UTC"
#> 

## read timestamps on a datasets
h5readTimestamps(example_file, name = "/DS1")
#> $access_time
#> [1] "1970-01-01 UTC"
#> 
#> $modification_time
#> [1] "1970-01-01 UTC"
#> 
#> $change_time
#> [1] "2018-01-11 18:31:45 UTC"
#> 
#> $birth_time
#> [1] "1970-01-01 UTC"
#> 
```
