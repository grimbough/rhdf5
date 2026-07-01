# Additional functions for finding details of dataset chunking.

Additional functions for finding details of dataset chunking.

## Usage

``` r
H5Dchunk_dims(h5dataset)

H5Dis_chunked(h5dataset)
```

## Arguments

- h5dataset:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset.

## Value

- `H5Dchunk_dims`: If the supplied dataset is chunked returns a vector,
  with length equal to the rank of the dataset, containing the size of
  the dataset dimensions. Returns `NULL` if the given dataset is not
  chunked.

- `H5Dis_chunked`: returns `TRUE` if a dataset is chunked and `FALSE`
  otherwise.

## Details

These functions do not map directly to the HDF5 C API but follow the
same style and are included as potentially useful additions.

- `H5Dis_chunked` tests whether a dataset is chunked.

- `H5Dchunk_dims` will return the dimensions of the dataset chunks.

## Author

Mike Smith

## Examples

``` r
file <- tempfile(fileext = ".h5")
fid <- H5Fcreate(file)

## Create a dataset that will be represented by 4 chunks if complete
h5createDataset(file, "data", dims = c(10, 10), chunk = c(5, 5), storage.mode = "integer")
#> Warning: An open HDF5 file handle exists. If the file has changed on disk meanwhile, the function may not work properly. Run 'h5closeAll()' to close all open HDF5 object handles.
did <- H5Dopen(fid, "data")

H5Dis_chunked(did)
#> [1] TRUE
H5Dchunk_dims(did)
#> [1] 5 5
```
