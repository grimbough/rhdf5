# Open an object in an HDF5 file

Open an object in an HDF5 file

## Usage

``` r
H5Oopen(h5loc, name)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)

- name:

  Path to the object to be opened. This should be relative to `h5loc`
  rather than the file.

## Value

An object of class
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
if the open operation was successful. `FALSE` otherwise.

## See also

[`H5Oclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oclose.md)

## Examples

``` r
h5File <- tempfile(pattern = "ex_H5O.h5")

# create an hdf5 file and write something
h5createFile(h5File)
h5createGroup(h5File, "foo")
B <- array(seq(0.1, 2.0, by = 0.1), dim = c(5, 2, 2))
h5write(B, h5File, "foo/B")

# reopen file and dataset and get object info
fid <- H5Fopen(h5File)
oid <- H5Oopen(fid, "foo")
H5Oget_num_attrs(oid)
#> [1] 0
H5Oclose(oid)
H5Fclose(fid)
```
