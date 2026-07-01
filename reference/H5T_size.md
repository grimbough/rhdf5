# Retrieve or set the type of padding used by string datatype

Retrieve or set the type of padding used by string datatype

## Usage

``` r
H5Tset_size(dtype_id = h5default(type = "H5T"), size)

H5Tget_size(dtype_id)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to query or modify.

- size:

  The new datatype size in bytes.

## Examples

``` r
tid <- H5Tcopy("H5T_C_S1")
H5Tset_size(tid, 3)
H5Tget_size(tid)
#> [1] 3
tid <- H5Tcopy("H5T_C_S1")
H5Tset_size(tid, 3)
H5Tget_size(tid)
#> [1] 3
```
