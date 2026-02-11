# Determine whether an identifier is valid

An identifier is no longer valid after it has been closed.

## Usage

``` r
H5Iis_valid(h5identifier)
```

## Arguments

- h5identifier:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md).

## Value

A logical of length 1. `TRUE` is the identifier is valid, `FALSE` if
not.

## Examples

``` r
h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
fid <- H5Fopen(h5file)

## test whether the identifer to the opened file is valid
H5Iis_valid(fid)
#> [1] TRUE

## the file ID is no longer valid after it has been closed
H5Fclose(fid)
H5Iis_valid(fid)
#> [1] FALSE
```
