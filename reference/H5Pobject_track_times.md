# Set whether to record timestamps for operations performed on an HDF5 object.

Set whether to record timestamps for operations performed on an HDF5
object.

## Usage

``` r
H5Pset_obj_track_times(h5plist, track_times = TRUE)

H5Pget_obj_track_times(h5plist)
```

## Arguments

- h5plist:

  An
  [H5IdComponent](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  object representing an object creation property list.

- track_times:

  `logical` specifying whether times associated with an object should
  recorded.

## Details

Objects created using high-level **rhdf5** functions like
[`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md)
will have this setting turned off. This was done to ensure otherwise
identical files returned the same md5 hash. This differs from the
default setting in HDF5, which is for objects to record the times
operations were performed on them.
