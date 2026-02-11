# Write data to an HDF5 attribute

Write data to an HDF5 attribute

## Usage

``` r
H5Awrite(h5attribute, buf)
```

## Arguments

- h5attribute:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an attribute. Normally created by
  [`H5Aopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aopen.md)
  or similar.

- buf:

  The data to be written.
