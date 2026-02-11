# Create or modify an HDF5 enum datatype

Create or modify an HDF5 enum datatype

## Usage

``` r
H5Tenum_create(dtype_id = "H5T_NATIVE_INT")

H5Tenum_insert(dtype_id, name, value)
```

## Arguments

- dtype_id:

  ID of HDF5 datatype to work with. For `H5Tenum_create`, this is the
  identifier of the base data type, and must be an integer e.g.
  `H5T_NATIVE_INT`. For `H5Tenum_insert` this will be a datatype
  identifier created by `H5Tenum_create`.

- name:

  The name of a the new enum member. This is analogous to a "level" in
  an R factor.

- value:

  The value of the new member. Must be compatible with the base datatype
  defined by `dtype_id`.

## Value

- `H5Tinsert_enum()` returns an character representing the H5 identifier
  of the new datatype.

- [`H5Tset_precision()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_precision.md)
  is called for its side-effect of modifying the existing datatype. It
  will invisibly return `TRUE` if this is successful `FALSE` if not.

## Examples

``` r
tid <- H5Tenum_create(dtype_id = "H5T_NATIVE_UCHAR")
H5Tenum_insert(tid, name = "TRUE", value = 1L)
H5Tenum_insert(tid, name = "FALSE", value = 0L)
```
