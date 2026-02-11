# Close access to an HDF5 file

Close access to an HDF5 file

## Usage

``` r
H5Fclose(h5file)
```

## Arguments

- h5file:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an HDF5 file ID. Typically created via
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md)
  or
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md).
