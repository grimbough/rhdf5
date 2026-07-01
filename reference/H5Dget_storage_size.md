# Find the amount of storage allocated for a dataset

`H5Dget_storage_size` returns the amount of storage, in bytes, allocated
in an HDF5 file to hold a given dataset. This is the amount of space
required on-disk, which not typically a good indicator of the amount of
memory that will be required to read the complete dataset.

## Usage

``` r
H5Dget_storage_size(h5dataset)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset

## Value

Returns an integer giving the number of bytes allocated in the file to
the dataset.

## Examples

``` r
f <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
fid <- H5Fopen(f)
did <- H5Dopen(fid, "DS1")
H5Dget_storage_size(did)
#> [1] 480
H5Dclose(did)
H5Fclose(fid)
```
