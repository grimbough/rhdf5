# Delete objects within a HDF5 file

Deletes the specified group or dataset from within an HDF5 file.

## Usage

``` r
h5delete(file, name)
```

## Arguments

- file:

  The filename (character) of the file in which the object is located.

- name:

  For `h5delete` the name of the object to be deleted. For
  `h5deleteAttribute` the name of the object to which the attribute
  belongs.

## Author

Mike Smith

## Examples

``` r
h5File <- tempfile(pattern = "ex_createFile.h5")

h5createFile(h5File)
h5createGroup(h5File, "foo")

h5ls(h5File)
#>   group name     otype dclass dim
#> 0     /  foo H5I_GROUP           

h5delete(h5File, "foo")
h5ls(h5File)
#> [1] group  name   otype  dclass dim   
#> <0 rows> (or 0-length row.names)
```
