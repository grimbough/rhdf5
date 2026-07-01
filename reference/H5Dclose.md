# Close an open HDF5 dataset

Close an open HDF5 dataset

## Usage

``` r
H5Dclose(h5dataset)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset

## Examples

``` r
h5file <- tempfile(fileext = ".h5")
h5createFile(h5file)
h5createDataset(h5file, dataset = "A", dims = 10)

fid <- H5Fopen(h5file)
did <- H5Dopen(h5loc = fid, name = "A")
did
#> HDF5 DATASET 
#>         name /A
#>     filename 
#>         type H5T_IEEE_F64LE
#>         rank 1
#>         size 10
#>      maxsize 10

## remember to close open handles
H5Dclose(did)
H5Fclose(fid)
```
