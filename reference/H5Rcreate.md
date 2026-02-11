# Create a reference

Creates a reference to an object or dataset selection inside an HDF5
file.

## Usage

``` r
H5Rcreate(h5loc, name, ref_type = "H5R_OBJECT", h5space = NULL)
```

## Arguments

- h5loc:

  An `H5IdComponent` object representing the location to be pointed to
  by the created reference.

- name:

  Character string giving the name of the object to be referenced,
  relative to the location given by `h5loc`.

- ref_type:

  The type of reference to create. Accepts either `H5R_OBJECT` or
  `H5R_DATASET_REGION`.

- h5space:

  An object of class `H5IdComponent` representing a dataspace with a
  selection set. This argument is only used if creating a reference to a
  dataset region, and will be ignored otherwise.

## Value

An
[H5Ref](https://huber-group-embl.github.io/rhdf5/reference/H5Ref-class.md)
object storing the reference.
