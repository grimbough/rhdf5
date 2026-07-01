# HDF5 General Library Functions

These low level functions provide general library functions for HDF5.

## Usage

``` r
H5open()

H5close()

H5garbage_collect()

H5get_libversion()
```

## Value

- `H5open` initializes the HDF5 library.

- `H5close` flushes all data to disk, closes all open identifiers, and
  cleans up memory.

- `H5garbage_collect` cleans up memory.

- `H5get_libversion` returns the version number of the HDF5 C-library.

## Author

Bernd Fischer, Mike Smith

## Examples

``` r
H5open()
H5close()
H5garbage_collect()
H5get_libversion()
#> majnum minnum relnum 
#>      1     14      6 
```
