# Set the size of a dataspace

Set the size of a dataspace

## Usage

``` r
H5Sset_extent_simple(h5space, dims, maxdims)
```

## Arguments

- h5space:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a dataspace.

- dims:

  Dimension of the dataspace. This argument is similar to the dim
  attribute of an array.

- maxdims:

  Maximum extension of the dimension of the dataset in the file. If not
  provided, it is set to `dims`.

  When viewing the HDF5 dataset with other software (e.g. HDFView), the
  dimensions appear in inverted order, because the fastest changing
  dimension in R is the first one, and in C it's the last one.
