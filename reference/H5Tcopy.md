# Copy an existing datatype

Copy an existing datatype

## Usage

``` r
H5Tcopy(dtype_id = h5default(type = "H5T"))
```

## Arguments

- dtype_id:

  Datatype to copy. Can either be a character specifying a predefined
  HDF5 datatype (see `h5const("H5T")` for valid options) or the ID of an
  already created datatype.
