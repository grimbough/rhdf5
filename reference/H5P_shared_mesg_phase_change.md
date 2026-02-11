# Get and set threshold values for storage of shared object header message indexes

Get and set threshold values for storage of shared object header message
indexes

## Usage

``` r
H5Pset_shared_mesg_phase_change(h5plist, max_list, min_btree)

H5Pget_shared_mesg_phase_change(h5plist)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the file creation property list

- max_list:

  Threshold above which storage shifts from list to B-tree

- min_btree:

  Threshold below which storage reverts to list format
