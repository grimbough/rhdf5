# Query dataset filter properties.

Return information about the filter pipeline applied to a dataset
creation property list.

## Usage

``` r
H5Pall_filters_avail(h5plist)

H5Pget_nfilters(h5plist)

H5Pget_filter(h5plist, idx)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- idx:

  Integer of length 1. This argument selects which filter to return
  information about. Indexing is R-style 1-based.

## Details

- `H5Pall_filters_avail()` checks whether all filters required to
  process a dataset are available to **rhdf5**. This can be required if
  reading files created with other HDF5 software.

- `H5Pget_nfilters()` returns the number of filters in the dataset chunk
  processing pipeline.

- `H5Pget_filter()` provides details of a specific filter in the
  pipeline. This includes the filter name and the parameters provided to
  it e.g. compression level.
