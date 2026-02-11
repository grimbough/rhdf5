# Create a copy of a dataspace

`H5S_copy()` creates an exact copy of a given dataspace.

## Usage

``` r
H5Scopy(h5space)
```

## Arguments

- h5space:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing the dataspace to be copied.

## Value

If the copying is successful returns an object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
representing the new dataspace. Otherwise returns `FALSE`.
