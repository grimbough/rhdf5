# Retrieve the name of an object from a given identifier

Retrieve the name of an object from a given identifier

## Usage

``` r
H5Iget_name(h5obj)
```

## Arguments

- h5obj:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md).
  Can represent a file, group, dataset or attribute.

## Examples

``` r
h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
fid <- H5Fopen(h5file)
gid <- H5Gopen(fid, "/")
did <- H5Dopen(gid, "DS1")

H5Iget_name(did)
#> [1] "/DS1"

## tidy up
H5Gclose(gid)
H5Fclose(fid)
```
