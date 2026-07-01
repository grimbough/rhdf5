# Determine whether a datatype is a variable length string

Determine whether a datatype is a variable length string

## Usage

``` r
H5Tis_variable_str(dtype_id)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to query.

## Examples

``` r
tid <- H5Tcopy("H5T_C_S1")
H5Tset_size(tid, 3)
H5Tis_variable_str(tid)
#> [1] FALSE
```
