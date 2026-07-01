# Delete attribute

Deletes an attribute associated with a group or dataset within an HDF5
file.

## Usage

``` r
h5deleteAttribute(file, name, attribute)
```

## Arguments

- file:

  The filename (character) of the file in which the object is located.

- name:

  The name of the object to which the attribute belongs.

- attribute:

  Name of the attribute to be deleted.

## Author

Mike Smith

## Examples

``` r
h5File <- tempfile(pattern = "ex_createAttribute.h5")
h5createFile(h5File)
h5write(1:1, h5File, "A")
fid <- H5Fopen(h5File)
did <- H5Dopen(fid, "A")
h5createAttribute(did, "time", c(1, 10))
#> [1] TRUE
h5readAttributes(h5File, "A")
#> Warning: An open HDF5 file handle exists. If the file has changed on disk meanwhile, the function may not work properly. Run 'h5closeAll()' to close all open HDF5 object handles.
#> $`rhdf5-NA.OK`
#> [1] 1
#> 
#> $time
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    0    0    0    0    0    0    0    0    0     0
#> 
h5deleteAttribute(h5File, "A", "time")
#> Warning: An open HDF5 file handle exists. If the file has changed on disk meanwhile, the function may not work properly. Run 'h5closeAll()' to close all open HDF5 object handles.
h5readAttributes(h5File, "A")
#> Warning: An open HDF5 file handle exists. If the file has changed on disk meanwhile, the function may not work properly. Run 'h5closeAll()' to close all open HDF5 object handles.
#> $`rhdf5-NA.OK`
#> [1] 1
#> 
H5Dclose(did)
H5Fclose(fid)
```
