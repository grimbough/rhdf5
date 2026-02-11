# Determine whether a dataspace is a simple dataspace

In HDF5 a dataspace is considered "simple" if it represents a regular
N-dimensional array of points. Currently (HDF 1.10.7) all dataspaces are
simple. Support for complex dataspaces is planned for future HDF
versions.

## Usage

``` r
H5Sis_simple(h5space)
```

## Arguments

- h5space:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a dataspace.
