# Delete objects within a HDF5 file

Deletes the specified group or dataset from within an HDF5 file.

## Usage

``` r
h5delete(file, name)
```

## Arguments

- file:

  The filename (character) of the file in which the object is located.

- name:

  For `h5delete` the name of the object to be deleted. For
  `h5deleteAttribute` the name of the object to which the attribute
  belongs.

## Author

Mike Smith
