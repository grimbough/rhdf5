# Description

This folder contains the script used to produce the example HDF5 files  found
in the inst/testfiles/ folder.  These are created using the 
reference implementation of the anndata library found in 
[anndata](https://github.com/scverse/anndata).

# Generating the data

## Using Docker

If you do not have a version of python and zarr readily available, you can
alternatively use the Dockerfile provided to generate the data.  The following
command will build a container called `zarr_tester`.

```
docker build -t anndata-nullable .   
```

To generate the data you then use the command below.  This will produce the
example files in the folder `/tmp/zarr` on your local filesystem.  Modify that
path in the call to `docker run` if it is not appropriate for your system.

```
mkdir /tmp/anndata-nullable
docker run --rm --name anndata-nullable --user ${UID}:${UID} \
  -v /tmp/anndata-nullable:/data \
  anndata-nullable
```
