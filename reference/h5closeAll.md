# Close open HDF5 handles

This functions can be used in two ways. Firstly, it can be passed one or
more
[H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
objects and it'll will try to close all of them regardless of the
whether they represent a file, group, dataset etc. This can be easier
than making multiple calls to
[`H5Fclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fclose.md),
[`H5Gclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gclose.md),
etc.

## Usage

``` r
h5closeAll(...)
```

## Arguments

- ...:

  One or more objects of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  which should be closed. If nothing is provided to the function, all
  open handles will be closed.

## Value

Doesn't return anything. Called for the side-effect of closing open HDF5
handles.

## Details

Secondly, occasionally references to HDF5 files, groups, datasets etc
can be created and not closed correctly. Maybe because a function
stopped before getting to the close statement, or the open handle was
not assigned to an R variable. If no arguments are provide this function
identifies all open handles and closes them.

## Author

Mike Smith

## Examples

``` r

## create an empty file and then re-open it
h5File <- tempfile(pattern = "ex_h5closeAll.h5")
h5createFile(h5File)
H5Fopen(h5File)

## list all open identifiers
h5listIdentifier()
#>       type name
#> 1 H5I_FILE    /

## close all open identifiers and verify
h5closeAll()
h5listIdentifier()
#> [1] type name
#> <0 rows> (or 0-length row.names)
```
