# Get and set shared object header message index properties

Get and set shared object header message index properties

## Usage

``` r
H5Pset_shared_mesg_index(
  h5plist,
  index_num,
  mesg_type_flags = h5default(type = "H5O_SHMESG_FLAG"),
  min_mesg_size
)

H5Pget_shared_mesg_index(h5plist, index_num)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the file creation property list

- index_num:

  Index being configured. Indices use C-style 0-based counting, so the
  first index will be numbered 0.

- mesg_type_flags:

  Character specifying the types of messages that may be stored in this
  index. Valid values can be found with
  `h5const(type = "H5O_SHMESG_FLAG")`

- min_mesg_size:

  Minimum message size

## Value

`H5Pget_shared_mesg_index()` returns a list of length 2. The first
element is the types of messages that may be stored in the index, the
second element is the minimum message size.
