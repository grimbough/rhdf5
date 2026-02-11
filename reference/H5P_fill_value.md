# Set the fill value for an HDF5 dataset

`H5Pset_fill_value` sets the fill value for a dataset in the dataset
creation property list.

## Usage

``` r
H5Pset_fill_value(h5plist, value)
```

## Arguments

- h5plist:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a dataset creation property list.

- value:

  The default fill value of the dataset. A vector of length 1.

## See also

[H5P_fill_time](https://huber-group-embl.github.io/rhdf5/reference/H5P_fill_time.md),[H5Pfill_value_defined](https://huber-group-embl.github.io/rhdf5/reference/H5Pfill_value_defined.md)
