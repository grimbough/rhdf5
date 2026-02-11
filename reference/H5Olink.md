# Create a hard link to an object in an HDF5 file

Create a hard link to an object in an HDF5 file

## Usage

``` r
H5Olink(h5obj, h5loc, newLinkName, lcpl = NULL, lapl = NULL)
```

## Arguments

- h5obj:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing the object to be linked to.

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing the location at which the object is to be linked. Can
  represent a file, group, dataset, datatype or attribute.

- newLinkName:

  Character string giving the name of the new link. This should be
  relative to `h5loc`.

- lcpl, lapl:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  objects representing link creation and link access property lists
  respectively. If left as `NULL` the default values for these will be
  used.

## See also

[H5Gcreate_anon](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate_anon.md)

## Examples

``` r
## Create a temporary copy of an example file, and open it
example_file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")
file.copy(example_file, tempdir())
#> [1] FALSE
h5_file <- file.path(tempdir(), "h5ex_t_array.h5")
fid <- H5Fopen(h5_file)

## create a new group without a location in the file
gid <- H5Gcreate_anon(fid)

## create link to newly create group
## relative to the file identifier
H5Olink(h5obj = gid, h5loc = fid, newLinkName = "foo")

## tidy up
H5Gclose(gid)
H5Fclose(fid)

## Check we now have a "/foo" group
h5ls(h5_file)
#>   group name       otype dclass dim
#> 0     /  DS1 H5I_DATASET  ARRAY   4
#> 1     /  DS2 H5I_DATASET  ARRAY   4
#> 2     /  foo   H5I_GROUP           
```
