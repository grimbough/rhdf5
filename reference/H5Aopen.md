# Open an attribute for an HDF5 object

Open an attribute for an HDF5 object

## Usage

``` r
H5Aopen(h5obj, name)

H5Aopen_by_name(h5obj, objname = ".", name)

H5Aopen_by_idx(
  h5obj,
  n,
  objname = ".",
  index_type = h5default("H5_INDEX"),
  order = h5default("H5_ITER")
)
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

- objname:

  The name of the object the attribute belongs to.

- n:

  Opens attribute number `n` in the given order and index. Indexing is
  C-style, base-0, so the first attribute is opened with `n=0`.

- index_type:

  See `h5const("H5_INDEX")` for possible arguments.

- order:

  See `h5const("H5_ITER")` for possible arguments.

## Value

An object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
representing a H5 attribute identifier.
