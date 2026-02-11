# Get and set the size of the symbol table B-tree 1/2 rank and the leaf node 1/2 size

Get and set the size of the symbol table B-tree 1/2 rank and the leaf
node 1/2 size

## Usage

``` r
H5Pset_sym_k(h5plist, ik, lk)

H5Pget_sym_k(h5plist)
```

## Arguments

- h5plist:

  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing the file creation property list

- ik:

  Symbol table B-tree 1/2 rank

- lk:

  Symbol table leaf node 1/2 size
