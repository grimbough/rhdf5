# Retrieve information about a group

Retrieve information about a group

## Usage

``` r
H5Gget_info(h5loc)

H5Gget_info_by_name(h5loc, group_name)

H5Gget_info_by_idx(
  h5loc,
  n,
  group_name = ".",
  index_type = h5default("H5_INDEX"),
  order = h5default("H5_ITER")
)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 group.

- group_name:

  An additional group name specifying the group for which information is
  sought. It is interpreted relative to `h5loc`.

- n:

  Position in the index of the group for which information is retrieved.

- index_type:

  See `h5const("H5_INDEX")` for possible arguments.

- order:

  See `h5const("H5_ITER")` for possible arguments.

## Value

A list with group information

## Examples

``` r
h5file <- system.file("testfiles", "multiple_dtypes.h5", package = "rhdf5")
fid <- H5Fopen(h5file)
gid <- H5Gopen(fid, "/foo")
gid
#> HDF5 GROUP 
#>         name /foo
#>     filename 
#> 
#>   name       otype  dclass   dim
#> 0    A H5I_DATASET INTEGER 1 x 7
#> 1    B H5I_DATASET FLOAT   2 x 9
H5Gget_info(gid)
#> $storage_type
#> [1] 0
#> 
#> $nlink
#> [1] 2
#> 
#> $max_corder
#> [1] 0
#> 
#> $mounted
#> [1] FALSE
#> 
H5Gclose(gid)

## the "get_info_by" functions take the H5 object that contains the
## group(s) of interest.  We can retrieve information by index or by name
H5Gget_info_by_idx(fid, 3)
#> $storage_type
#> [1] 0
#> 
#> $nlink
#> [1] 2
#> 
#> $max_corder
#> [1] 0
#> 
#> $mounted
#> [1] FALSE
#> 
H5Gget_info_by_name(fid, "/foo")
#> $storage_type
#> [1] 0
#> 
#> $nlink
#> [1] 2
#> 
#> $max_corder
#> [1] 0
#> 
#> $mounted
#> [1] FALSE
#> 

H5Fclose(fid)
```
