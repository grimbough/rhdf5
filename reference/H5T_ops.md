# Get details of HDF5 data types

Get details of HDF5 data types

## Usage

``` r
H5Tget_class(dtype_id)

H5Tget_nmembers(dtype_id)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to work with. Normally created with a function
  like `H5Tcopy` or `H5Tenum_create`.

## Value

- `H5Tget_class()` returns an character vector of length 1 giving the
  class of the data type.

- `H5Tget_nmembers()` returns the number of members in the given
  datatype. Will fail with an error if the supplied datatype is not of
  type `H5T_COMPUND` or `H5T_ENUM`.

## Examples

``` r
## create an enum datatype with two entries
tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR")
H5Tenum_insert(tid, name = "TRUE", value = 1L)
H5Tenum_insert(tid, name = "FALSE", value = 0L)

H5Tget_class(tid)
#> [1] "H5T_ENUM"
H5Tget_nmembers(tid)
#> [1] 2
```
