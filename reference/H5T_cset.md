# Retrieve or set the character set to be used in a string datatype.

Retrieve or set the character set to be used in a string datatype.

## Usage

``` r
H5Tset_cset(dtype_id, cset = "ASCII")

H5Tget_cset(dtype_id)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to query or modify.

- cset:

  Encoding to use for string types. Valid options are 'ASCII' and
  'UTF-8'.

## Examples

``` r
tid <- H5Tcopy("H5T_C_S1")
H5Tget_cset(tid)
#> [1] 0
H5Tset_cset(tid, "UTF-8")
#> [1] 0
H5Tget_cset(tid)
#> [1] 1
```
