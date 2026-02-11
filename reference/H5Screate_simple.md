# Create a simple dataspace

Create a simple dataspace

## Usage

``` r
H5Screate_simple(dims, maxdims, native = FALSE)
```

## Arguments

- dims:

  A numeric vector defining the initial dimensions of the dataspace. The
  length of `dims` determines the rank of the dataspace.

- maxdims:

  A numeric vector with the same length length as `dims`. Specifies the
  upper limit on the size of the dataspace dimensions. Only needs to be
  specified if this is different from the values given to `dims`.

- native:

  An object of class `logical`. If `TRUE`, array-like objects are
  treated as stored in HDF5 row-major rather than R column-major
  orientation. Using `native = TRUE` increases HDF5 file portability
  between programming languages. A file written with `native = TRUE`
  should also be read with `native = TRUE`.

## Value

Returns an object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
representing a dataspace.

## See also

[H5Screate](https://huber-group-embl.github.io/rhdf5/reference/H5Screate.md)
