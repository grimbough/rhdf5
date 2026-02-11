# Add the BLOSC filter to the chunk processing pipeline.

Add the BLOSC filter to the chunk processing pipeline.

## Usage

``` r
H5Pset_blosc(h5plist, h5tid, method = 1L, level = 6L, shuffle = TRUE)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- h5tid:

  HDF5 data type id

- method:

  Integer defining which of the compression algorithms provided by BLOSC
  should be used. (See the details section for the mapping between
  integers and algorithms).

- level:

  Compression level to be used by the selected algorithm.

- shuffle:

  Logical defining whether the bit-shuffle algorithm should be used
  prior to compression. This makes use of the shuffle implementation
  provide by BLOSC, rather than the HDF5 version.
