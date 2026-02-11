# Add a filter to the dataset filter pipeline.

Add a filter to the dataset filter pipeline.

## Usage

``` r
H5Pset_filter(h5plist, filter_id, is_mandatory = FALSE, cd_values)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- filter_id:

  Integer of length 1, giving the ID of the filter to be used.

- is_mandatory:

  Logical of length 1. Filters can be either optional or mandatory. If
  this argument is set to `FALSE` the filter won't be applied to a chunk
  in the case of failure, but the data will still be written. Setting to
  `TRUE` will result in a failure when writing the dataset if the filter
  fails for some reason.

- cd_values:

  Integer vector giving parameters to be supplied to the filter. No
  guidance is given for the number of values supplied here, it is
  specific to each filter and the user is expected to know appropriate
  options for the requested filter.
