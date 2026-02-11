# Open an existing HDF5 dataset

Open an existing HDF5 dataset

## Usage

``` r
H5Dopen(h5loc, name, dapl = NULL)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group).

- name:

  Name of the dataset to open.

- dapl:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 dataset access property list.

## Value

An object of class `H5IdComponent` representing the opened dataset. To
prevent memory leaks this must be closed with a call to
[`H5Dclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dclose.md)
when no longer needed.

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

## rember to close open handles
H5Dclose(did)
H5Fclose(fid)
```
