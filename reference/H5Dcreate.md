# Create a new HDF5 dataset

Create a new HDF5 dataset

## Usage

``` r
H5Dcreate(
  h5loc,
  name,
  dtype_id,
  h5space,
  lcpl = NULL,
  dcpl = NULL,
  dapl = NULL
)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group). See
  [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md),
  [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md),
  [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
  [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  to create an object of this kind.

- name:

  Name of the dataset.

- dtype_id:

  A character name of a datatype. See `h5const("H5T")` for possible
  datatypes. Can also be an integer representing an HDF5 datatype.

- h5space:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 dataspace. See
  [`H5Dget_space()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_space.md),
  [`H5Screate_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate_simple.md),
  [`H5Screate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate.md)
  to create an object of this kind

- lcpl, dcpl, dapl:

  An objects of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing HDF5 property lists. Specially these should respectively
  be: a link creation property list, a dataset creation property list, a
  dataset access property list

## Value

An object of class `H5IdComponent` representing the opened dataset.
