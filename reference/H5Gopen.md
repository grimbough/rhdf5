# Open a specified group

Open a specified group

## Usage

``` r
H5Gopen(h5loc, name)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 file or group that contains the group to be opened.

- name:

  Name of the group to open.

## Value

An object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
representing the opened group. When access to the group is no longer
needed this should be released with
[`H5Gclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gclose.md)
to prevent resource leakage.

## See also

[`H5Gclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gclose.md)
