# Open a reference object.

Given a reference and the file to which that reference applies,
`H5Rdeference()` will open the reference object and return an
identifier.

## Usage

``` r
H5Rdereference(ref, h5loc)
```

## Arguments

- ref:

  `H5ref` object containing the reference to be opened.

- h5loc:

  An `H5IdComponent` object representing the file containing the
  referenced object.

## Value

An object of class `H5IdComponent` representing the opened object
referenced by `ref`. This should be closed with the appropriate function
e.g.
[`H5Dclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dclose.md),
[`H5Oclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oclose.md),
etc. when no longer needed.

## Details

If `ref` contains more than one reference, only the first reference will
be used. It must be subset with `[` if one of the other stored
references should be opened.
