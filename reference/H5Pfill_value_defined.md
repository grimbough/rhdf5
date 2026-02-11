# Determine whether a property list has a fill value defined

Determine whether a property list has a fill value defined

## Usage

``` r
H5Pfill_value_defined(h5plist)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

## Value

`TRUE` if the fill value is defined, `FALSE` if not. Will return `NULL`
if there is a problem determining the status of the fill value.

## Details

Note that the return value for this function is slightly different from
the C version. The C API provides three return types and can, in the
case that a fill value is defined, differentiate whether the value is
the HDF5 library default or has been set by the application.
