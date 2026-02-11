# Perform operation between an existing selection and an another hyperslab definition.

Combines a hyperslab selection specified by `start`, `stride`, `count`
and `block` arguments with the current selection for the dataspace
represented by `h5space`.

## Usage

``` r
H5Scombine_hyperslab(
  h5space,
  op = h5default("H5S_SELECT"),
  start = NULL,
  stride = NULL,
  count = NULL,
  block = NULL
)
```

## Arguments

- h5space:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a dataspace.

- op:

  Character string defined the operation used to join the two
  dataspaces. See `h5const("H5S_SELECT")` for the list of available
  options.

- start, stride, count, block:

  Integer vectors, each with length equal to the rank of the dataspace.
  These parameters define the new hyperslab to select.

## Value

An
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
object representing a new dataspace with the generated selection.

## See also

[`H5Scombine_select()`](https://huber-group-embl.github.io/rhdf5/reference/H5Scombine_select.md),
[`H5Sselect_hyperslab()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sselect_hyperslab.md)

## Examples

``` r
## create a 1 dimensional dataspace
sid_1 <- H5Screate_simple(dims = 20)

## select a single block of 5 points in sid_1
## this is equivalent to [11:16] in R syntax
H5Sselect_hyperslab(sid_1,
  start = 11, stride = 1,
  block = 5, count = 1
) #

## combine the existing selection with a new
## selection consisting of 2 blocks each of 1 point
## equivalent to [c(3,5)] in R syntax
sid_2 <- H5Scombine_hyperslab(sid_1,
  op = "H5S_SELECT_OR",
  start = 3, stride = 2,
  block = 1, count = 2
)

## confirm we have selected 5 in our original dataspace
## and 7 points in the newly created dataspace
H5Sget_select_npoints(sid_1)
#> [1] 5
H5Sget_select_npoints(sid_2)
#> [1] 7

## tidy up
H5Sclose(sid_1)
H5Sclose(sid_2)
```
