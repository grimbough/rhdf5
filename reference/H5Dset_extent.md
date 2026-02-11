# Change the dimensions of an HDF5 dataset

Change the dimensions of an HDF5 dataset

## Usage

``` r
H5Dset_extent(h5dataset, size)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset.

- size:

  An integer vector with the new dimension of the dataset.

## Value

A logical vector of length 1. Value will be `TRUE` if the operation was
sucessful and `FALSE` otherwise.

## Details

This function can only be applied to datasets that meet the following
criteria:

- A chunked dataset with unlimited dimensions

- A chunked dataset with fixed dimensions if the new dimension sizes are
  less than the maximum sizes set with maxdims

## Author

Bernd Fischer, Mike Smith
