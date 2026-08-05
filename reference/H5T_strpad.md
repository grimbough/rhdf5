# Retrieve or set the type of padding used by string datatype

Retrieve or set the type of padding used by string datatype

## Usage

``` r
H5Tset_strpad(dtype_id, strpad = "NULLPAD")

H5Tget_strpad(dtype_id)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to query or modify.

- strpad:

  Character vector of length 1 specifying the type of padding to use.
  Valid options are `NULLTERM`, `NULLPAD` and `SPACEPAD`.

## Examples

``` r
tid <- H5Tcopy("H5T_C_S1")
H5Tget_strpad(tid)
#> [1] 0
H5Tset_strpad(tid, "NULLPAD")
#> [1] 0
H5Tget_strpad(tid)
#> [1] 1
```
