# Add the BZIP2 filter to the chunk processing pipeline.

Add the BZIP2 filter to the chunk processing pipeline.

## Usage

``` r
H5Pset_bzip2(h5plist, level = 2L)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- level:

  Compression level to be used by the selected algorithm.
