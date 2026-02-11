# Get the number of chunks in a dataset

Retrieves the number of chunks used by an HDF5 dataset.

## Usage

``` r
H5Dget_num_chunks(h5dataset)
```

## Arguments

- h5dataset:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing the dataset from which chunks will be counted.

## Value

An integer value indicating the number of chunks present in the dataset
or selected region.

## Details

Note, this function only returns the number of chunks that actually have
data written to them. It does not return the theoretical number of
chunks in a dataset or intersection with a dataspace. For example, if an
empty dataset is created and but no values have been written to it
`H5Dget_num_chunks()` will return 0. This can be seen in the examples
below.

The C API also provides an optional parameter to constrain the query by
providing a dataspace selection. However this argument is not currently
used at the C level and so is ommitted here.

## Examples

``` r
file <- tempfile(fileext = ".h5")
fid <- H5Fcreate(file)

## Create a dataset that will be represented by 4 chunks if complete
h5createDataset(file, "data", dims = c(10, 10), chunk = c(5, 5), storage.mode = "integer")
#> Warning: An open HDF5 file handle exists. If the file has changed on disk meanwhile, the function may not work properly. Run 'h5closeAll()' to close all open HDF5 object handles.
did <- H5Dopen(fid, "data")
## Here we return 0 chunks as no values have been written
H5Dget_num_chunks(did)
#> [1] 0

## Now write data to half the dataset
h5writeDataset(obj = matrix(1:50, nrow = 10), h5loc = fid, name = "/data", index = list(1:10, 1:5))
## We now see it contains 2 chunks
H5Dget_num_chunks(did)
#> [1] 2

## Now write the complete dataset, overwriting the existing values
h5writeDataset(obj = matrix(201:300, nrow = 10), h5loc = fid, name = "/data", index = NULL)
## We now see it contains 4 chunks
H5Dget_num_chunks(did)
#> [1] 4

## Tidy up op handles
h5closeAll(did, fid)
```
