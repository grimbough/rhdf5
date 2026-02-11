# Add the shuffle filter to the chunk processing pipeline.

Add the shuffle filter to the chunk processing pipeline.

## Usage

``` r
H5Pset_shuffle(h5plist)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

## Value

Returns (invisibly) an integer vector of length 1. The only element of
this vector will be non-negative if the filter was set successfully and
negative otherwise.
