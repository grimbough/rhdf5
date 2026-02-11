# An S4 class representing H5 references.

A class representing one or more HDF5 references.

## Usage

``` r
# S4 method for class 'H5Ref'
show(object)

# S4 method for class 'H5Ref'
length(x)

# S4 method for class 'H5Ref'
c(x, ...)

# S4 method for class 'H5Ref'
x[i]
```

## Arguments

- object:

  Object of class `H5Ref`

- x:

  An `H5Ref` object.

- ...:

  Additional `H5Ref` objects to be combined with `x`.

- i:

  Integer vector giving the indices of references to select.

## Details

The length of the `val` slot is dependent on both the number and type of
references stored in the object. `H5R_OBJECT` references are stored in 8
bytes, while `H5R_DATASET_REGION` references require 12 bytes. The
length of `val` will then be a multiple of 8 or 12 respectively. This
also means that references of different types cannot be combined in a
single object.

## Methods (by generic)

- `show(H5Ref)`: Print details of the object to screen.

- `length(H5Ref)`: Return the number of references stored in an `H5Ref`
  object.

- `c(H5Ref)`: Combine two or more `H5Ref` objects. Objects must all
  contain the same type of reference, either `H5R_OBJECT` or
  `H5R_DATASET_REFERENCE`.

- `[`: Subset an `H5Ref` object.

## Slots

- `val`:

  `raw` vector containing the byte-level representation of each
  reference.

- `type`:

  `integer` of length 1, which maps to either `H5R_OBJECT` or
  `H5R_DATASET_REGION`.
