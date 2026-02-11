# Create an attribute for an HDF5 object

Creates an attribute, `name`, which is attached to the object specified
by the identifier `h5obj`. The attribute name must be unique for the
object.

## Usage

``` r
H5Acreate(h5obj, name, dtype_id, h5space)
```

## Arguments

- h5obj:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 object identifier (file, group, or dataset). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md),
  [`H5Dcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dcreate.md),
  or
  [`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md)
  to create an object of this kind.

- name:

  The name of the attribute (character).

- dtype_id:

  A character name of a datatype. See `h5const("H5T")` for possible
  datatypes. Can also be an integer representing an HDF5 datatype. Only
  simple datatypes are allowed for attributes.

- h5space:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 dataspace. See
  [`H5Dget_space()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_space.md),
  [`H5Screate_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate_simple.md),
  [`H5Screate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate.md)
  to create an object of this kind.

## Value

An object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
representing a H5 attribute identifier.
