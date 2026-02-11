# Find the type of an object

Possible types returned by the function are:

- `H5I_FILE`

- `H5I_GROUP`

- `H5I_DATATYPE`

- `H5I_DATASPACE`

- `H5I_DATASET`

- `H5I_ATTR`

## Usage

``` r
H5Iget_type(h5identifier)
```

## Arguments

- h5identifier:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md).

## Value

Returns a character vector of length 1 containing the HDF5 type for the
supplied identifier.

## Examples

``` r
h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
fid <- H5Fopen(h5file)
gid <- H5Gopen(fid, "/")

## identify the HDF5 types for these identifiers
H5Iget_type(fid)
#> [1] "H5I_FILE"
H5Iget_type(gid)
#> [1] "H5I_GROUP"

## tidy up
H5Gclose(gid)
H5Fclose(fid)
```
