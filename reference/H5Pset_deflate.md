# Add the deflate compression filter to the chunk processing pipeline.

Valid values for the compression level range from 0 (no compression) to
9 (best compression, slowest speed). Note that applying this function
with `level = 0` does not mean the filter is removed. It is still part
of the filter pipeline, but no compression is performed. The filter will
still need to be available on any system that reads a file created with
this setting

## Usage

``` r
H5Pset_deflate(h5plist, level)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- level:

  Integer giving the compression level to use. Valid values are from 0
  to 9.
