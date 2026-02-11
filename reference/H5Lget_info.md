# Find information about a link

`H5Lget_info()` identifies the type of link specified by the the `h5loc`
and `name` arguments. This is more limited than the equivalent function
in the standard HDF5 library.

## Usage

``` r
H5Lget_info(h5loc, name)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group).

- name:

  The name of the link to be queried.

## Value

A character vector of length 1 giving the type of link. Possible values
are: `H5L_TYPE_HARD`, `H5L_TYPE_SOFT`, `H5L_TYPE_EXTERNAL`,
`H5L_TYPE_ERROR`
