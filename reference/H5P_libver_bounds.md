# Control the range of HDF5 library versions that will be compatible with a file.

Control the range of HDF5 library versions that will be compatible with
a file.

## Usage

``` r
H5Pset_libver_bounds(
  h5plist,
  libver_low = "H5F_LIBVER_EARLIEST",
  libver_high = "H5F_LIBVER_LATEST"
)

H5Pget_libver_bounds(h5plist)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a file access property list.

- libver_low, libver_high:

  Define the earliest and latest versions of the HDF5 library that will
  be used when writing object in the file.
