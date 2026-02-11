# Find the number of attributes associated with an HDF5 object

Find the number of attributes associated with an HDF5 object

## Usage

``` r
H5Oget_num_attrs(h5obj)

H5Oget_num_attrs_by_name(h5loc, name)
```

## Arguments

- h5obj:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 object identifier (file, group, or dataset).

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group).

- name:

  The name of the object to be checked.

## Value

Returns a vector of length 1 containing the number of attributes the
specified object has.

## Details

These functions are not part of the standard HDF5 C API.
