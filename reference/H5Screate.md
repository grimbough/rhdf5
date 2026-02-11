# Create a new dataspace of a specified type

Create a new dataspace of a specified type

## Usage

``` r
H5Screate(type = h5default("H5S"), native = FALSE)
```

## Arguments

- type:

  The type of dataspace to create. See `h5const("H5S")` for possible
  types.

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

[H5Screate_simple](https://huber-group-embl.github.io/rhdf5/reference/H5Screate_simple.md)
