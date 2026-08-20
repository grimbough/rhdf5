# Return a copy of the HDF5 datatype for a dataset

Return a copy of the HDF5 datatype for a dataset

## Usage

``` r
H5Dget_type(h5dataset)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset

## Examples

``` r
f <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
fid <- H5Fopen(f)
did <- H5Dopen(fid, "DS1")
type <- H5Dget_type(did)
type
#> [1] "216172782113783916"
H5Dclose(did)
H5Fclose(fid)
```
