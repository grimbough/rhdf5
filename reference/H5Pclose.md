# Close and release a property list

`H5Pclose()` terminates access to a property list. All property lists
should be closed when they no longer need to be accessed. This frees
resources used by the property list. Failing to call `H5Pclose()` can
lead to memory leakage over time.

## Usage

``` r
H5Pclose(h5plist)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the property list to close.
