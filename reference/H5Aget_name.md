# Get the name of an HDF5 attribute object

Retrieves the name of the attribute specified by an HDF5 attribute
object.

## Usage

``` r
H5Aget_name(h5attribute)
```

## Arguments

- h5attribute:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an attribute. Normally created by
  [`H5Aopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aopen.md)
  or similar.

## Value

A character vector of length 1 containing the name of the attribute.
