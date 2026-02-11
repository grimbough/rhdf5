# Write data to dataset

Write data to dataset

## Usage

``` r
H5Dwrite(h5dataset, buf, h5type = NULL, h5spaceMem = NULL, h5spaceFile = NULL)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset.

- buf:

  The R object containing the data to be written to the dataset.

- h5type:

  Datatype of the HDF5 dataset to be written. If left as `NULL` it will
  use the dataype of the R object supplied to `buf`.

- h5spaceMem, h5spaceFile:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  objects representing the memory and file dataspaces respectively. If
  these are left `NULL` dataspaces that match the size and shape of
  `h5dataset` will be used.
