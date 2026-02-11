# Return selection for a reference to dataset region

Given a dataset region reference, this function will return the
dataspace and selection required to read the data points indicated by
the reference.

## Usage

``` r
H5Rget_region(ref, h5loc)
```

## Arguments

- ref:

  An object of class `H5Ref`. This function is only valid for reference
  of type `H5R_DATASET_REGION`, and not `H5R_OBJECT`.

- h5loc:

  An `H5IdComponent` object representing the file containing the
  referenced object.

## Value

An object of class `H5IdComponent` representing the dataspace of the
dataset that `ref` points to. The dataspace will have the selection set
that matches the selection pointed to by `ref`. This should be closed
using
[`H5Sclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sclose.md)
when no longer required.
