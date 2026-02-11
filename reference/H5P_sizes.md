# Get and set the sizes of offsets and lengths used in an HDF5 file

Get and set the sizes of offsets and lengths used in an HDF5 file

## Usage

``` r
H5Pset_sizes(h5plist, sizeof_addr, sizeof_size)

H5Pget_sizes(h5plist)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the file creation property list

- sizeof_addr:

  Offset size in bytes

- sizeof_size:

  Length size in bytes
