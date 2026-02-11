# Identify the type of object that a reference points to

Identify the type of object that a reference points to

## Usage

``` r
H5Rget_obj_type(ref, h5loc)
```

## Arguments

- ref:

  `H5ref` object containing the reference to be queried.

- h5loc:

  An `H5IdComponent` object representing the file containing the
  referenced object.

## Value

Character string of length 1 identifying the object type. Valid return
values are: `"GROUP"`, `"DATASET"`, and `"NAMED_DATATYPE"`.
