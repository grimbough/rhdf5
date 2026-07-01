# Retrieves the metadata for an HDF5 object specified by an identifier.

Retrieves the metadata for an HDF5 object specified by an identifier.

## Usage

``` r
H5Oget_info(h5loc)
```

## Arguments

- h5loc:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an open HDF5 dataset or group.

## Examples

``` r

## Create a temporary copy of an example file check the contents
example_file <- system.file("testfiles", "h5ex_t_array.h5", package = "rhdf5")

## open the example file, root group, and DS1 dataset
fid <- H5Fopen(example_file)
gid <- H5Gopen(fid, "/")
did <- H5Dopen(fid, "/DS1")

## List the available object information for both groups and datasets
H5Oget_info(h5loc = gid)
#> $file_num
#> [1] 43
#> 
#> $type
#> [1] "GROUP"
#> 
#> $reference_count
#> [1] 1
#> 
#> $access_time
#> [1] "1970-01-01 UTC"
#> 
#> $modification_time
#> [1] "1970-01-01 UTC"
#> 
#> $change_time
#> [1] "1970-01-01 UTC"
#> 
#> $birth_time
#> [1] "1970-01-01 UTC"
#> 
#> $num_attrs
#> [1] 0
#> 

H5Oget_info(h5loc = did)
#> $file_num
#> [1] 43
#> 
#> $type
#> [1] "DATASET"
#> 
#> $reference_count
#> [1] 1
#> 
#> $access_time
#> [1] "1970-01-01 UTC"
#> 
#> $modification_time
#> [1] "1970-01-01 UTC"
#> 
#> $change_time
#> [1] "2018-01-11 18:31:45 UTC"
#> 
#> $birth_time
#> [1] "1970-01-01 UTC"
#> 
#> $num_attrs
#> [1] 0
#> 

## close open handles
h5closeAll(did, gid, fid)
```
