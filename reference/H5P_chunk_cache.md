# Set parameters for the raw data chunk cache

Set parameters for the raw data chunk cache

## Usage

``` r
H5Pset_chunk_cache(h5plist, rdcc_nslots, rdcc_nbytes, rdcc_w0)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset access property list.

- rdcc_nslots:

  Integer defining the number of chunk slots in the raw data chunk cache
  for this dataset.

- rdcc_nbytes:

  Integer setting the total size of the raw data chunk cache for this
  dataset in bytes. In most cases increasing this number will improve
  performance, as long as you have enough free memory. The default size
  is 1 MB

- rdcc_w0:

  Numeric value defining the chunk preemption policy. Must be between
  `0` and `1` inclusive.
