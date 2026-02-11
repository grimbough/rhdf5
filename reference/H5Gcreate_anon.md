# Create a new HDF5 group without linking it into a file

Create a new HDF5 group without linking it into a file

## Usage

``` r
H5Gcreate_anon(h5loc)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  specifying the file in which the new group is to be created.

## Value

`H5Gcreate_anon` returns an object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
representing the newly created group. However at this point is is still
anonymous, and must be linked into the file structure via
[`H5Olink()`](https://huber-group-embl.github.io/rhdf5/reference/H5Olink.md).
If this is not done, the group will be deleted from the file when it is
closed.

## See also

[`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md),
[`H5Olink()`](https://huber-group-embl.github.io/rhdf5/reference/H5Olink.md)
