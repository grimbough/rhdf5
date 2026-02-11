# Copy a link from one location to another

Copy a link from one location to another

## Usage

``` r
H5Lcopy(h5loc, name, h5loc_dest, name_dest, lcpl = NULL, lapl = NULL)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group) where the new
  link is placed.

- name:

  The name of the link to be copied.

- h5loc_dest:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing the destination file or group where a copied or moved
  link should be created.

- name_dest:

  The name of the link to be created when copying or moving.

- lcpl, lapl:

  Link creation and link access property lists. If left as `NULL` the
  HDF5 defaults will be used.
