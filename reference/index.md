# Package index

## About rhdf5

- [`rhdf5`](https://huber-group-embl.github.io/rhdf5/reference/rhdf5.md)
  : rhdf5: An interface between HDF5 and R

## High level functions

Wrappers around several low-level functions

- [`h5testFileLocking()`](https://huber-group-embl.github.io/rhdf5/reference/h5_FileLocking.md)
  [`h5disableFileLocking()`](https://huber-group-embl.github.io/rhdf5/reference/h5_FileLocking.md)
  [`h5enableFileLocking()`](https://huber-group-embl.github.io/rhdf5/reference/h5_FileLocking.md)
  : Test and set file locking for HDF5
- [`h5createFile()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createFile.md)
  : Create HDF5 file
- [`h5createGroup()`](https://huber-group-embl.github.io/rhdf5/reference/h5_createGroup.md)
  : Create HDF5 group
- [`h5delete()`](https://huber-group-embl.github.io/rhdf5/reference/h5_delete.md)
  : Delete objects within a HDF5 file
- [`h5deleteAttribute()`](https://huber-group-embl.github.io/rhdf5/reference/h5_deleteAttribute.md)
  : Delete attribute
- [`h5dump()`](https://huber-group-embl.github.io/rhdf5/reference/h5_dump.md)
  : Dump the content of an HDF5 file.
- [`h5errorHandling()`](https://huber-group-embl.github.io/rhdf5/reference/h5_errorHandling.md)
  : Set how HDF5 error messages are displayed
- [`h5read()`](https://huber-group-embl.github.io/rhdf5/reference/h5_read.md)
  : Reads and write object in HDF5 files
- [`h5readAttributes()`](https://huber-group-embl.github.io/rhdf5/reference/h5_readAttributes.md)
  : Read all attributes from a given location in an HDF5 file
- [`h5save()`](https://huber-group-embl.github.io/rhdf5/reference/h5_save.md)
  : Saves a one or more objects to an HDF5 file.
- [`h5set_extent()`](https://huber-group-embl.github.io/rhdf5/reference/h5_set_extent.md)
  : Set a new dataset extension
- [`h5write()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md)
  [`h5writeDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5_write.md)
  : Write object to an HDF5 file.
- [`h5writeAttribute()`](https://huber-group-embl.github.io/rhdf5/reference/h5_writeAttribute.md)
  : Write an R object as an HDF5 attribute
- [`h5closeAll()`](https://huber-group-embl.github.io/rhdf5/reference/h5closeAll.md)
  : Close open HDF5 handles
- [`h5const()`](https://huber-group-embl.github.io/rhdf5/reference/h5constants.md)
  [`h5constType()`](https://huber-group-embl.github.io/rhdf5/reference/h5constants.md)
  [`h5default()`](https://huber-group-embl.github.io/rhdf5/reference/h5constants.md)
  : HDF5 library constants.
- [`h5createAttribute()`](https://huber-group-embl.github.io/rhdf5/reference/h5createAttribute.md)
  : Create HDF5 attribute
- [`h5createDataset()`](https://huber-group-embl.github.io/rhdf5/reference/h5createDataset.md)
  : Create HDF5 dataset
- [`h5listIdentifier()`](https://huber-group-embl.github.io/rhdf5/reference/h5listObjects.md)
  [`h5validObjects()`](https://huber-group-embl.github.io/rhdf5/reference/h5listObjects.md)
  : List all open HDF5 objects.
- [`h5ls()`](https://huber-group-embl.github.io/rhdf5/reference/h5ls.md)
  : List the content of an HDF5 file.
- [`h5readTimestamps()`](https://huber-group-embl.github.io/rhdf5/reference/h5readTimestamps.md)
  : Read the time stamps associated with an HDF5 group or dataset.
- [`h5version()`](https://huber-group-embl.github.io/rhdf5/reference/h5version.md)
  : Print the rhdf5 and libhdf5 version numbers

## HDF5 Files

Functions for working with HDF5 Files

- [`H5Fclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fclose.md)
  : Close access to an HDF5 file
- [`H5Fcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fcreate.md)
  : Create an HDF5 file
- [`H5Fflush()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fflush.md)
  : Flush all buffers associated with a file to disk
- [`H5Fget_filesize()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fget_filesize.md)
  : Find the size of an open HDF5 file
- [`H5Fget_intent()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fget_intent.md)
  : Determine the read only or read/write status of an open file handle.
- [`H5Fget_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fget_name.md)
  : Retrieve the name of the file to which an object belongs
- [`H5Fget_create_plist()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fget_plist.md)
  [`H5Fget_access_plist()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fget_plist.md)
  : Get property lists associated with an HDF5 file
- [`H5Fis_hdf5()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fis_hdf5.md)
  : Determine whether a file is in the HDF5 format
- [`H5Fopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Fopen.md)
  : Open an existing HDF5 file

## HDF5 Groups

Functions for working with HDF5 Groups

- [`H5Gclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gclose.md)
  : Close a specified group
- [`H5Gcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate.md)
  : Create a new HDF5 group and link it to a location in a file
- [`H5Gcreate_anon()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gcreate_anon.md)
  : Create a new HDF5 group without linking it into a file
- [`H5Gget_info()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gget_info.md)
  [`H5Gget_info_by_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gget_info.md)
  [`H5Gget_info_by_idx()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gget_info.md)
  : Retrieve information about a group
- [`H5Gopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Gopen.md)
  : Open a specified group

## HDF5 Datasets

Functions for working with HDF5 Datasets

- [`H5Dchunk_dims()`](https://huber-group-embl.github.io/rhdf5/reference/H5D_extras.md)
  [`H5Dis_chunked()`](https://huber-group-embl.github.io/rhdf5/reference/H5D_extras.md)
  : Additional functions for finding details of dataset chunking.
- [`H5Dclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dclose.md)
  : Close an open HDF5 dataset
- [`H5Dcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dcreate.md)
  : Create a new HDF5 dataset
- [`H5Dget_create_plist()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_create_plist.md)
  : Return a copy of the dataset creation property list for a dataset
- [`H5Dget_num_chunks()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_num_chunks.md)
  : Get the number of chunks in a dataset
- [`H5Dget_space()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_space.md)
  : Return a copy of the HDF5 dataspace for a dataset
- [`H5Dget_storage_size()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_storage_size.md)
  : Find the amount of storage allocated for a dataset
- [`H5Dget_type()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dget_type.md)
  : Return a copy of the HDF5 datatype for a dataset
- [`H5Dopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dopen.md)
  : Open an existing HDF5 dataset
- [`H5Dread()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dread.md)
  : Read from an HDF5 dataset
- [`H5Dset_extent()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dset_extent.md)
  : Change the dimensions of an HDF5 dataset
- [`H5Dwrite()`](https://huber-group-embl.github.io/rhdf5/reference/H5Dwrite.md)
  : Write data to dataset

## HDF5 Dataspaces

Functions for working with HDF5 Dataspaces

- [`H5Sclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sclose.md)
  : Close and release a dataspace

- [`H5Scombine_hyperslab()`](https://huber-group-embl.github.io/rhdf5/reference/H5Scombine_hyperslab.md)
  : Perform operation between an existing selection and an another
  hyperslab definition.

- [`H5Scombine_select()`](https://huber-group-embl.github.io/rhdf5/reference/H5Scombine_select.md)
  : Combine two selections

- [`H5Scopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Scopy.md)
  : Create a copy of a dataspace

- [`H5Screate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate.md)
  : Create a new dataspace of a specified type

- [`H5Screate_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Screate_simple.md)
  : Create a simple dataspace

- [`H5Sget_select_npoints()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sget_select_npoints.md)
  : Find the number of elements in a dataspace selection

- [`H5Sget_simple_extent_dims()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sget_simple_extent_dims.md)
  : Find the size of a dataspace

- [`H5Sis_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sis_simple.md)
  : Determine whether a dataspace is a simple dataspace

- [`H5Sselect_all()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sselect_all.md)
  : Set the selection region of a dataspace to include all elements

- [`H5Sselect_hyperslab()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sselect_hyperslab.md)
  : Perform operation between an existing selection and an another
  hyperslab definition.

- [`H5Sselect_index()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sselect_index.md)
  : Select elements of a dataspace using R-style indexing

- [`H5Sselect_none()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sselect_none.md)
  : Set the selection region of a dataspace to include no elements

- [`H5Sselect_valid()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sselect_valid.md)
  : Check that a selection is valid

- [`H5Sset_extent_simple()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sset_extent_simple.md)
  : Set the size of a dataspace

- [`H5Sunlimited()`](https://huber-group-embl.github.io/rhdf5/reference/H5Sunlimited.md)
  :

  Retrieve value for `H5S_UNLIMITED` constant

## HDF5 Attributes

Functions for working with HDF5 Attributes

- [`H5Aclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aclose.md)
  : Close an HDF5 attribute
- [`H5Acreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Acreate.md)
  : Create an attribute for an HDF5 object
- [`H5Adelete()`](https://huber-group-embl.github.io/rhdf5/reference/H5Adelete.md)
  : Delete an specified attribute of an HDF5 object
- [`H5Aexists()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aexists.md)
  : Check whether an specific attribute exists for an HDF5 object
- [`H5Aget_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aget_name.md)
  : Get the name of an HDF5 attribute object
- [`H5Aget_space()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aget_space.md)
  : Get a copy of the attribute dataspace
- [`H5Aget_type()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aget_type.md)
  : Get a copy of the attribute datatype
- [`H5Aopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aopen.md)
  [`H5Aopen_by_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aopen.md)
  [`H5Aopen_by_idx()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aopen.md)
  : Open an attribute for an HDF5 object
- [`H5Aread()`](https://huber-group-embl.github.io/rhdf5/reference/H5Aread.md)
  : Read data from an HDF5 attribute
- [`H5Awrite()`](https://huber-group-embl.github.io/rhdf5/reference/H5Awrite.md)
  : Write data to an HDF5 attribute

## HDF5 Objects

Functions for working with HDF5 Objects

- [`H5Oclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oclose.md)
  : Close an HDF5 object
- [`H5Ocopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Ocopy.md)
  : Copies an HDF5 object
- [`H5Oget_info()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oget_info.md)
  : Retrieves the metadata for an HDF5 object specified by an
  identifier.
- [`H5Oget_num_attrs()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oget_num_attrs.md)
  [`H5Oget_num_attrs_by_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oget_num_attrs.md)
  : Find the number of attributes associated with an HDF5 object
- [`H5Olink()`](https://huber-group-embl.github.io/rhdf5/reference/H5Olink.md)
  : Create a hard link to an object in an HDF5 file
- [`H5Oopen()`](https://huber-group-embl.github.io/rhdf5/reference/H5Oopen.md)
  : Open an object in an HDF5 file

## HDF5 Links

Functions for working with HDF5 Links

- [`H5Lcopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Lcopy.md)
  : Copy a link from one location to another
- [`H5Lcreate_external()`](https://huber-group-embl.github.io/rhdf5/reference/H5Lcreate_external.md)
  : Create a link to an object in a different HDF5 file
- [`H5Ldelete()`](https://huber-group-embl.github.io/rhdf5/reference/H5Ldelete.md)
  : Remove a link from a group
- [`H5Lexists()`](https://huber-group-embl.github.io/rhdf5/reference/H5Lexists.md)
  : Confirm existence of a link
- [`H5Lget_info()`](https://huber-group-embl.github.io/rhdf5/reference/H5Lget_info.md)
  : Find information about a link
- [`H5Lmove()`](https://huber-group-embl.github.io/rhdf5/reference/H5Lmove.md)
  : Move a link within an HDF5 file

## HDF5 Identifiers

Functions for working with HDF5 Identifiers

- [`show(`*`<H5IdComponent>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  [`` `&`( ``*`<H5IdComponent>`*`,`*`<character>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  [`` `$`( ``*`<H5IdComponent>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  [`` `$<-`( ``*`<H5IdComponent>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  [`` `[`( ``*`<H5IdComponent>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  [`` `[<-`( ``*`<H5IdComponent>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5IdComponent-class.md)
  : An S4 class representing an H5 object
- [`H5Iget_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Iget_name.md)
  : Retrieve the name of an object from a given identifier
- [`H5Iget_type()`](https://huber-group-embl.github.io/rhdf5/reference/H5Iget_type.md)
  : Find the type of an object
- [`H5Iis_valid()`](https://huber-group-embl.github.io/rhdf5/reference/H5Iis_valid.md)
  : Determine whether an identifier is valid

## HDF5 Datatypes

Functions for working with HDF5 Datatypes

- [`H5Tset_cset()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_cset.md)
  [`H5Tget_cset()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_cset.md)
  : Retrieve or set the character set to be used in a string datatype.
- [`H5Tenum_create()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_enum.md)
  [`H5Tenum_insert()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_enum.md)
  : Create or modify an HDF5 enum datatype
- [`H5Tget_class()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_ops.md)
  [`H5Tget_nmembers()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_ops.md)
  : Get details of HDF5 data types
- [`H5Tset_precision()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_precision.md)
  [`H5Tget_precision()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_precision.md)
  : Retrieve or set the precision of an HDF5 datatype
- [`H5Tset_size()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_size.md)
  [`H5Tget_size()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_size.md)
  : Retrieve or set the type of padding used by string datatype
- [`H5Tset_strpad()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_strpad.md)
  [`H5Tget_strpad()`](https://huber-group-embl.github.io/rhdf5/reference/H5T_strpad.md)
  : Retrieve or set the type of padding used by string datatype
- [`H5Tcopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Tcopy.md)
  : Copy an existing datatype
- [`H5Tis_variable_str()`](https://huber-group-embl.github.io/rhdf5/reference/H5Tis_variable_str.md)
  : Determine whether a datatype is a variable length string

## HDF5 Property Lists

### File Creation Properties

- [`H5Pget_version()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pget_version.md)
  : Get version information for objects in a file creation property list
- [`H5Pset_shared_mesg_nindexes()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_shared_mesg_nindexes.md)
  [`H5Pget_shared_mesg_nindexes()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_shared_mesg_nindexes.md)
  : Get and set the number of object header message indexes
- [`H5Pset_shared_mesg_phase_change()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_shared_mesg_phase_change.md)
  [`H5Pget_shared_mesg_phase_change()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_shared_mesg_phase_change.md)
  : Get and set threshold values for storage of shared object header
  message indexes
- [`H5Pset_istore_k()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_istore_k.md)
  [`H5Pget_istore_k()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_istore_k.md)
  : Get and set the 1/2 rank of an indexed storage B-tree
- [`H5Pset_shared_mesg_index()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pshared_mesg_index.md)
  [`H5Pget_shared_mesg_index()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pshared_mesg_index.md)
  : Get and set shared object header message index properties
- [`H5Pset_sizes()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_sizes.md)
  [`H5Pget_sizes()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_sizes.md)
  : Get and set the sizes of offsets and lengths used in an HDF5 file
- [`H5Pset_sym_k()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_sym_k.md)
  [`H5Pget_sym_k()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_sym_k.md)
  : Get and set the size of the symbol table B-tree 1/2 rank and the
  leaf node 1/2 size
- [`H5Pset_userblock()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_userblock.md)
  [`H5Pget_userblock()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_userblock.md)
  : Get and set the user block size

### File Access Properties

- [`H5Pset_libver_bounds()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_libver_bounds.md)
  [`H5Pget_libver_bounds()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_libver_bounds.md)
  : Control the range of HDF5 library versions that will be compatible
  with a file.
- [`H5Pset_fapl_ros3()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_fapl_ros3.md)
  : Set the read-only S3 virtual file driver

### Group Creation Properties

- [`H5Pset_create_intermediate_group()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_create_intermediate_group.md)
  [`H5Pget_create_intermediate_group()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_create_intermediate_group.md)
  : Get and set whether to create missing intermediate groups

### Object Creation Properties

- [`H5Pset_obj_track_times()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pobject_track_times.md)
  [`H5Pget_obj_track_times()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pobject_track_times.md)
  : Set whether to record timestamps for operations performed on an HDF5
  object.

### Dataset Creation Properties

- [`H5Pall_filters_avail()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_filters.md)
  [`H5Pget_nfilters()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_filters.md)
  [`H5Pget_filter()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_filters.md)
  : Query dataset filter properties.
- [`H5Pset_shuffle()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_shuffle.md)
  : Add the shuffle filter to the chunk processing pipeline.
- [`H5Pset_nbit()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_nbit.md)
  : Add the N-Bit filter to the chunk processing pipeline.
- [`H5Pset_deflate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_deflate.md)
  : Add the deflate compression filter to the chunk processing pipeline.
- [`H5Pset_szip()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_szip.md)
  : Add the SZIP compression filter to the chunk processing pipeline.
- [`H5Pset_bzip2()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_bzip2.md)
  : Add the BZIP2 filter to the chunk processing pipeline.
- [`H5Pset_blosc()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_blosc.md)
  : Add the BLOSC filter to the chunk processing pipeline.
- [`H5Pset_lzf()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_lzf.md)
  : Add the LZF filter to the chunk processing pipeline.
- [`H5Pset_filter()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_filter.md)
  : Add a filter to the dataset filter pipeline.
- [`H5Pset_chunk()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_chunk.md)
  [`H5Pget_chunk()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_chunk.md)
  : Get and set the size of the chunks used to store a chunked layout
  dataset
- [`H5Pset_chunk_cache()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_chunk_cache.md)
  : Set parameters for the raw data chunk cache
- [`H5Pset_layout()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_layout.md)
  [`H5Pget_layout()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_layout.md)
  : Get and set the type of storage used to store the raw data for a
  dataset
- [`H5Pclose()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pclose.md)
  : Close and release a property list
- [`H5Pcopy()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pcopy.md)
  : Copy an existing property list to create a new property list
- [`H5Pcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pcreate.md)
  : Create a new HDF5 property list
- [`H5Pget_class()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pget_class.md)
  : Return the property list class identifier for a property list
- [`H5Pset_fill_value()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_fill_value.md)
  : Set the fill value for an HDF5 dataset
- [`H5Pset_fill_time()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_fill_time.md)
  [`H5Pget_fill_time()`](https://huber-group-embl.github.io/rhdf5/reference/H5P_fill_time.md)
  : Set the time when fill values are written to a dataset
- [`H5Pfill_value_defined()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pfill_value_defined.md)
  : Determine whether a property list has a fill value defined
- [`H5Pset_fapl_ros3()`](https://huber-group-embl.github.io/rhdf5/reference/H5Pset_fapl_ros3.md)
  : Set the read-only S3 virtual file driver

## HDF5 References

- [`H5R`](https://huber-group-embl.github.io/rhdf5/reference/H5R.md) :
  H5R - References to objects and regions
- [`H5Rcreate()`](https://huber-group-embl.github.io/rhdf5/reference/H5Rcreate.md)
  : Create a reference
- [`H5Rdereference()`](https://huber-group-embl.github.io/rhdf5/reference/H5Rdereference.md)
  : Open a reference object.
- [`show(`*`<H5Ref>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5Ref-class.md)
  [`length(`*`<H5Ref>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5Ref-class.md)
  [`c(`*`<H5Ref>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5Ref-class.md)
  [`` `[`( ``*`<H5Ref>`*`)`](https://huber-group-embl.github.io/rhdf5/reference/H5Ref-class.md)
  : An S4 class representing H5 references.
- [`H5Rget_name()`](https://huber-group-embl.github.io/rhdf5/reference/H5Rget_name.md)
  : Return the name of the object that a reference points to
- [`H5Rget_obj_type()`](https://huber-group-embl.github.io/rhdf5/reference/H5Rget_obj_type.md)
  : Identify the type of object that a reference points to
- [`H5Rget_region()`](https://huber-group-embl.github.io/rhdf5/reference/H5Rget_region.md)
  : Return selection for a reference to dataset region

## HDF5 Filters

- [`H5Zfilter_avail()`](https://huber-group-embl.github.io/rhdf5/reference/H5Zfilter_avail.md)
  : Determine whether a filter is available on this system

## H5 Library Functions

- [`H5open()`](https://huber-group-embl.github.io/rhdf5/reference/H5functions.md)
  [`H5close()`](https://huber-group-embl.github.io/rhdf5/reference/H5functions.md)
  [`H5garbage_collect()`](https://huber-group-embl.github.io/rhdf5/reference/H5functions.md)
  [`H5get_libversion()`](https://huber-group-embl.github.io/rhdf5/reference/H5functions.md)
  : HDF5 General Library Functions
