# Select elements of a dataspace using R-style indexing

Combines a hyperslab selection specified by `start`, `stride`, `count`
and `block` arguments with the current selection for the dataspace
represented by `h5space`.

## Usage

``` r
H5Sselect_index(h5space, index)
```

## Arguments

- h5space:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a dataspace.

- index:

  A list of integer indices. The length of the list corresponds to the
  number of dimensions of the HDF5 array. If a list element is `NULL`,
  all elements of the respective dimension are selected.

## Details

`H5Sselect_hyperslab` is similar to, but subtly different from,
[`H5Scombine_hyperslab()`](https://huber-group-embl.github.io/rhdf5/reference/H5Scombine_hyperslab.md).
The former modifies the selection of the dataspace provided in the
`h5space` argument, while the later returns a new dataspace with the
combined selection.

## Examples

``` r
## create a 1 dimensional dataspace
sid <- H5Screate_simple(c(10, 5, 3))

## Select elements that lie in in the rows 1-3, columns 2-4,
## and the entire 3rd dimension
H5Sselect_index(sid, list(1:3, 2:4, NULL))

## We can check the number of selected points.
## This should be 27 (3 * 3 * 3)
H5Sget_select_npoints(sid)
#> [1] 27

## always close dataspaces after usage to free resources
H5Sclose(sid)
```
