# Retrieve or set the precision of an HDF5 datatype

Retrieve or set the precision of an HDF5 datatype

## Usage

``` r
H5Tset_precision(dtype_id, precision)

H5Tget_precision(dtype_id)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to set precision of.

- precision:

  The number of bytes of precision for the datatype.

## Value

- `H5Tget_precision()` returns an integer giving the number of
  significant bits used by the given datatype.

- `H5Tset_precision()` is call for its side-effect of modifying the
  precision of a datatype. It will invisibly return `TRUE` if this is
  successful and will stop with an error if the operation fails.

## Examples

``` r
tid <- H5Tcopy("H5T_NATIVE_INT")
H5Tget_precision(tid)
#> [1] 32
H5Tset_precision(tid, 16L)
H5Tget_precision(tid)
#> [1] 16
```
