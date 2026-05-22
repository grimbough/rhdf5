# Set how HDF5 error messages are displayed

Sets the options for handling HDF5 error messages in the R sessions.

## Usage

``` r
h5errorHandling(type = "normal")
```

## Arguments

- type:

  'normal' (default) shows a one line error message in R. 'verbose'
  shows the whole HDF5 error message. 'suppress' suppresses the HDF5
  error messages completely.

## Value

Returns 0 if options are set successfully.

## See also

[rhdf5](https://huber-group-embl.github.io/rhdf5/reference/rhdf5.md)

## Author

Bernd Fischer

## Examples

``` r

h5errorHandling("normal")
```
