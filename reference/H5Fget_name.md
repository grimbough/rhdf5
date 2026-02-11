# Retrieve the name of the file to which an object belongs

Retrieve the name of the file to which an object belongs

## Usage

``` r
H5Fget_name(h5obj)
```

## Arguments

- h5obj:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md).
  Despite this being an H5F function, it works equally well on H5 file,
  group, dataset and attribute datatypes.

## Examples

``` r
## use an example file and show its location
h5file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
h5file
#> [1] "/home/runner/work/_temp/Library/rhdf5/testfiles/h5ex_t_array.h5"

## open a file handle and confirm we can identify the file it points to
fid <- H5Fopen(h5file)
H5Fget_name(fid)
#> [1] "/home/runner/work/_temp/Library/rhdf5/testfiles/h5ex_t_array.h5"

## H5Fget_name() can be applied to group and dataset handles too
gid <- H5Gopen(fid, name = "/")
did <- H5Dopen(fid, name = "DS1")
H5Fget_name(gid)
#> [1] "/home/runner/work/_temp/Library/rhdf5/testfiles/h5ex_t_array.h5"
H5Fget_name(did)
#> [1] "/home/runner/work/_temp/Library/rhdf5/testfiles/h5ex_t_array.h5"

## tidy up
H5Dclose(did)
H5Gclose(gid)
H5Fclose(fid)
```
