# Return the name of the object that a reference points to

Return the name of the object that a reference points to

## Usage

``` r
H5Rget_name(ref, h5loc)
```

## Arguments

- ref:

  `H5ref` object containing the reference to be queried.

- h5loc:

  An `H5IdComponent` object representing the file containing the
  referenced object.

## Value

Character string of length 1 giving the name of the referenced object.
