# Create a link to an object in a different HDF5 file

`H5Lcreate_external()` creates a new external link. An external link is
a soft link to an object in a different HDF5 file from the location of
the link.

## Usage

``` r
H5Lcreate_external(target_file_name, target_obj_name, link_loc, link_name)
```

## Arguments

- target_file_name:

  Name of the external HDF5 to link to

- target_obj_name:

  Path to the object in the file specified by `target_file_name` to link
  to.

- link_loc:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object giving the location where the new link should be created. Can
  represent an HDF5 file or group.

- link_name:

  Name (path) of the new link, relative to the location of `link_loc`.

## Examples

``` r
## The example below creates a new HDF5 file in a temporary director, and then
## links to the group "/foo" found in the file "multiple_dtypes.h5"
## distributed with the package.

h5File1 <- system.file("testfiles", "multiple_dtypes.h5", package = "rhdf5")
h5File2 <- tempfile(pattern = "H5L_2_", fileext = ".h5")
h5createFile(h5File2)

## open the new file & create a link to the group "/foo" in the original file
fid <- H5Fopen(h5File2)
H5Lcreate_external(
  target_file_name = h5File1, target_obj_name = "/foo",
  link_loc = fid, link_name = "/external_link"
)
H5Fclose(fid)

## check the new file has a group called "/external_link"
h5ls(h5File2)
#>            group          name       otype  dclass   dim
#> 0              / external_link   H5I_GROUP              
#> 1 /external_link             A H5I_DATASET INTEGER 1 x 7
#> 2 /external_link             B H5I_DATASET   FLOAT 2 x 9
```
