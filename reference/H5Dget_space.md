# Return a copy of the HDF5 dataspace for a dataset

Return a copy of the HDF5 dataspace for a dataset

## Usage

``` r
H5Dget_space(h5dataset)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset

## Value

Returns an object of class `H5IdComponent` representing a HDF5 dataspace
identifier

## Examples

``` r
f <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
fid <- H5Fopen(f)
did <- H5Dopen(fid, "DS1")
space <- H5Dget_space(did)
space
#> HDF5 DATASPACE 
#>         rank 1
#>         size 4
#>      maxsize 4
H5Dclose(did)
H5Fclose(fid)
```
