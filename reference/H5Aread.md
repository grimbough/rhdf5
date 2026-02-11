# Read data from an HDF5 attribute

Read data from an HDF5 attribute

## Usage

``` r
H5Aread(h5attribute, buf = NULL, bit64conversion = c("int", "double", "bit64"))
```

## Arguments

- h5attribute:

  An object of class
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  representing an attribute. Normally created by
  [`H5Aopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aopen.md)
  or similar.

- buf:

  Optional buffer to store retrieved values. The buffer size has to fit
  the size of the memory space `h5spaceMem`. No extra memory will be
  allocated for the data. Default is `NULL` which means the function
  will return the attribute data.

- bit64conversion:

  Defines how 64-bit integers are converted. (See the details section
  for more information on these options.)

## Value

If `buf=NULL` returns the contents of the attribute. Otherwise return 0
if attribute is read successfully.

## Details

Internally, R does not support 64-bit integers. All integers in R are
32-bit integers. By setting bit64conversion='int', a coercing to 32-bit
integers is enforced, with the risk of data loss, but with the insurance
that numbers are represented as integers. bit64conversion='double'
coerces the 64-bit integers to floating point numbers. doubles can
represent integers with up to 54-bits, but they are not represented as
integer values anymore. For larger numbers there is again a data loss.
bit64conversion='bit64' is recommended way of coercing. It represents
the 64-bit integers as objects of class 'integer64' as defined in the
package 'bit64'. Make sure that you have installed 'bit64'. The datatype
'integer64' is not part of base R, but defined in an external package.
This can produce unexpected behaviour when working with the data.
