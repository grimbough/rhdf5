# Get property lists associated with an HDF5 file

Get property lists associated with an HDF5 file

## Usage

``` r
H5Fget_create_plist(h5file)

H5Fget_access_plist(h5file)
```

## Arguments

- h5file:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 file identifier. Typically produced by
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
  or
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md).
