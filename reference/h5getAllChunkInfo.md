# Return detailed information on chunks of an HDF5 dataset

Returns the offset, filter mask, file address, and byte size of every
stored chunk in a chunked HDF5 dataset. This is a specific application
of the HDF5 `H5Dchunk_iter` function and requires that the dataset uses
chunked storage; contiguous or compact datasets will raise an error.

## Usage

``` r
h5getAllChunkInfo(h5dataset)
```

## Arguments

- h5dataset:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset, as returned by
  [`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md).

## Value

A named list with four elements, one entry per stored chunk:

- offset:

  Numeric matrix with one row per chunk and one column per dataset
  dimension, giving the chunk's logical origin in element coordinates.

- filter_mask:

  Numeric vector of per-chunk filter pipeline bitmasks. A value of `0`
  means all filters were applied.

- addr:

  Numeric vector of byte offsets within the HDF5 file at which each
  chunk's data begins.

- size:

  Numeric vector of compressed (on-disk) byte sizes for each chunk.

## See also

[`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md),
[`H5Dget_num_chunks()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_num_chunks.md)

## Examples

``` r
## chunked dataset: szip-compressed 2-D integer array
h5file <- system.file("testfiles", "h5ex_d_szip.h5", package = "rhdf5")
fid <- H5Fopen(h5file, flags = "H5F_ACC_RDONLY")
did <- H5Dopen(fid, "DS1")

ci <- h5getAllChunkInfo(did)
head(ci$offset)   # logical chunk origins (element coordinates)
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    8
#> [3,]    0   16
#> [4,]    0   24
#> [5,]    0   32
#> [6,]    0   40
ci$addr[1]        # file offset of first chunk in bytes
#> [1] 4016
ci$size[1]        # compressed size of first chunk in bytes
#> [1] 59

H5Dclose(did)
H5Fclose(fid)
```
