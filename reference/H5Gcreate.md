# Create a new HDF5 group and link it to a location in a file

`H5Gcreate` is used to a new group and link it into a file.

## Usage

``` r
H5Gcreate(h5loc, name)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)

- name:

  Name of the new group to be created.
