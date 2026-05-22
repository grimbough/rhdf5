# Remove a link from a group

Remove a link from a group

## Usage

``` r
H5Ldelete(h5loc, name)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing a H5 location identifier (file or group).

- name:

  The name of the link to be deleted.

## Examples

``` r

h5file <- tempfile(pattern = "_ex_H5L.h5")

# create an hdf5 file and a group
h5createFile(h5file)
h5createGroup(h5file, "/foo")

# reopen file and confirm "/foo" exists but "/baa" does not
fid <- H5Fopen(h5file)
H5Lexists(fid, "/foo")
#> [1] TRUE

# remove the link to "/foo" and confirm it no longer exists
H5Ldelete(fid, "/foo")
H5Lexists(fid, "/foo")
#> [1] FALSE

H5Fclose(fid)
```
