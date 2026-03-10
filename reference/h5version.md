# Print the rhdf5 and libhdf5 version numbers

Returns the version number of the Bioconductor package rhdf5 and the
C-library libhdf5.

## Usage

``` r
h5version()
```

## Value

A list of major, minor and release number.

## Author

Bernd Fischer, Mike L. Smith

## Examples

``` r
h5version()
#> This is Bioconductor rhdf5 2.55.15 linking to C-library HDF5 1.10.7 (Rhdf5lib version: 1.32.0) and rhdf5filters 1.22.0
```
