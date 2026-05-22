# Combine two selections

Combine two selections

## Usage

``` r
H5Scombine_select(h5space1, op = h5default("H5S_SELECT"), h5space2)
```

## Arguments

- h5space1, h5space2:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  objects representing a dataspaces.

- op:

  Character string defined the operation used to join the two
  dataspaces. See `h5const("H5S_SELECT")` for the list of available
  options.

## Value

Returns an
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
object representing a new dataspace. The new dataspace will have the
same extent as `h5space1` with the hyperslab selection being the result
of combining the selections of `h5space1` and `h5space2`.

## See also

[`H5Scombine_hyperslab()`](https://huber-group-embl.github.io/rhdf5/reference/H5Scombine_hyperslab.md)

## Examples

``` r

## create two 1 dimensional dataspaces
## of different sizes
sid_1 <- H5Screate_simple(dims = 20)
sid_2 <- H5Screate_simple(dims = 10)

## select a single block of 5 points in sid_1
## this is equivalent to [11:16] in R syntax
H5Sselect_hyperslab(sid_1,
  start = 11, stride = 1,
  block = 5, count = 1
)

## select 2 blocks of 1 point from sid_2
## equivalent to [c(3,5)] in R syntax
H5Sselect_hyperslab(sid_2,
  start = 3, stride = 2,
  block = 1, count = 2
)

## confirm we have select 5 and 2 points resepectively
H5Sget_select_npoints(sid_1)
#> [1] 5
H5Sget_select_npoints(sid_2)
#> [1] 2

## combine the two dataset selections keeping points that
## are in one or both of the selections
sid_3 <- H5Scombine_select(sid_1, "H5S_SELECT_OR", sid_2)

## extent of the new dataset is the same as sid_1
sid_3
#> HDF5 DATASPACE 
#>         rank 1
#>         size 20
#>      maxsize 20
## confirm the selection contains 7 points
H5Sget_select_npoints(sid_3)
#> [1] 7

## tidy up
H5Sclose(sid_1)
H5Sclose(sid_2)
H5Sclose(sid_3)
```
