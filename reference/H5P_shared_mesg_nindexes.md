# Get and set the number of object header message indexes

Get and set the number of object header message indexes

## Usage

``` r
H5Pset_shared_mesg_nindexes(h5plist, nindexes)

H5Pget_shared_mesg_nindexes(h5plist)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the file creation property list

- nindexes:

  Number of shared object header message indexes to be available in
  files
