# Add the SZIP compression filter to the chunk processing pipeline.

Add the SZIP compression filter to the chunk processing pipeline.

## Usage

``` r
H5Pset_szip(h5plist, options_mask, pixels_per_block)
```

## Arguments

- h5plist:

  Object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- options_mask, pixels_per_block:

  Integer vectors of length 1, setting parameters of the SZIP algorithm.
  See <https://portal.hdfgroup.org/display/HDF5/H5P_SET_SZIP> for more
  details.

## References

<https://portal.hdfgroup.org/display/HDF5/Szip+Compression+in+HDF+Products>
