# Get and set the size of the chunks used to store a chunked layout dataset

Get and set the size of the chunks used to store a chunked layout
dataset

## Usage

``` r
H5Pset_chunk(h5plist, dim)

H5Pget_chunk(h5plist)
```

## Arguments

- h5plist:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- dim:

  The chunk size used to store the dataset. This argument should be an
  integer vector of the same length as the number of dimensions of the
  dataset the dataset creation property list will be applied to.

## Details

Note that a necessary side effect of running this function is that the
layout of the dataset will be changes to `H5D_CHUNKED` if it is not
already set to this.

## See also

[`H5Pset_layout()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_layout.md)
