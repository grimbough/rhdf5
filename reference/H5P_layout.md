# Get and set the type of storage used to store the raw data for a dataset

Possible options for the `layout` argument are:

- `H5D_COMPACT`

- `H5D_CONTIGUOUS`

- `H5D_CHUNKED`

- `H5D_VIRTUAL`

## Usage

``` r
H5Pset_layout(h5plist, layout = h5default("H5D"))

H5Pget_layout(h5plist)
```

## Arguments

- h5plist:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- layout:

  A character giving the name of a dataset layout type.

## Details

The names of the layout types can also be obtained via `h5const("H5D")`.
