# Open an existing HDF5 file

Open an existing HDF5 file

## Usage

``` r
H5Fopen(name, flags = h5default("H5F_ACC_RD"), fapl = NULL, native = FALSE)
```

## Arguments

- name:

  The name (or path) of the HDF5 file to be opened.

- flags:

  Character string defining the access mode for opening the file.

- fapl:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing a file access property list. Leaving this argument
  as `NULL` will use the default HDF5 properties.

- native:

  An object of class `logical`. If `TRUE`, array-like objects are
  treated as stored in HDF5 row-major rather than R column-major
  orientation. Using `native = TRUE` increases HDF5 file portability
  between programming languages. A file written with `native = TRUE`
  should also be opened for reading with `native = TRUE`.

## Details

Possible values for the `flags` argument are `H5F_ACC_RDWR` and
`H5F_ACC_RDONLY`. Note that HDF5's "Single Write Multiple Reader (SWMR)
mode is not currently supported via **rhdf5**.
