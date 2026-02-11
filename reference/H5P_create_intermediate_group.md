# Get and set whether to create missing intermediate groups

Get and set whether to create missing intermediate groups

## Usage

``` r
H5Pset_create_intermediate_group(h5plist, create_groups = TRUE)

H5Pget_create_intermediate_group(h5plist)
```

## Arguments

- h5plist:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a link creation property list.

- create_groups:

  A logical of length 1 specifying whether missing groups should be
  created when a new object is created. Default is `TRUE`.

## Examples

``` r
pid <- H5Pcreate("H5P_LINK_CREATE")

## by default intermediate groups are not created
H5Pget_create_intermediate_group(pid)
#> [1] FALSE

## Change the setting so groups will be created

H5Pget_create_intermediate_group(pid)
#> [1] FALSE

## tidy up
H5Pclose(pid)
```
