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
