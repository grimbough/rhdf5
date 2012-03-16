\name{rhdf5}

\alias{rhdf5}
\alias{hdf5}
\alias{HDF5}
\alias{h5r}
\alias{H5}

\docType{package}

\title{Package overview}

\description{
\code{rhdf5} is an interface to the HDF5 library. The R-package contains the complete HDF5 library, thus no further installation of external packages is necessary. 

There are a number of high level R functions that provide a convinient way of accessing HDF5 file as well as R interfaces to a number of functions in the C-library.
}

\section{Package content}{

  HDF5 file, group, dataset creation
  \itemize{
    \item \code{\link{h5createFile}}
    \item \code{\link{h5createGroup}}
    \item \code{\link{h5createDataset}}
  }
  
  HDF5 file content listing
  \itemize{
    \item \code{\link{h5ls}}
    \item \code{\link{h5dump}}
  }
  
  Reading and writing data
  \itemize{
    \item \code{\link{h5read}}, \code{\link{h5write}}
    \item \code{\link{h5dump}}, \code{\link{h5save}}
  }
  
  HDF5 constants
  \itemize{
    \item \code{\link{h5const}}, \code{\link{h5default}}, \code{\link{h5constType}}
  }

  Low level interface to HDF5 C-library (for expert users only!):
  \itemize{
    \item HDF5 File Interface (\code{\link{H5Fcreate}}, \code{\link{H5Fopen}} / \code{\link{H5Fclose}} / \code{\link{H5Fflush}})
    \item HDF5 Group Interface (\code{\link{H5Gcreate}}, \code{\link{H5Gcreate_anon}}, \code{\link{H5Gopen}} / \code{\link{H5Gclose}} / \code{\link{H5Gget_info}}, \code{\link{H5Gget_info_by_idx}}, \code{\link{H5Gget_info_by_name}})
    \item HDF5 Link Interface (\code{\link{H5Lexists}}, \code{\link{H5Lget_info}})
    \item HDF5 Object Interface (\code{\link{H5Oopen}}, \code{\link{H5Oclose}}, \code{\link{H5Oget_num_attrs}}, \code{\link{H5Oget_num_attrs_by_name}})
    \item HDF5 Identifier Interface (\code{\link{H5Iget_type}}, \code{\link{H5Iget_name}}, \code{\link{H5Iis_valid}})
    \item HDF5 Dataset Interface (\code{\link{H5Dcreate}}, \code{\link{H5Dopen}} / \code{\link{H5Dclose}} / \code{\link{H5Dget_space}} / \code{\link{H5Dread}}, \code{\link{H5Dwrite}})
    \item HDF5 Attrbiute Interface (\code{\link{H5Acreate}}, \code{\link{H5Aopen}}, \code{\link{H5Aopen_by_idx}}, \code{\link{H5Aopen_by_name}} / \code{\link{H5Aclose}}, \code{\link{H5Adelete}} / \code{\link{H5Aexists}} / \code{\link{H5Aget_name}}, \code{\link{H5Aget_space}}, \code{\link{H5Aget_type}} / \code{\link{H5Aread}}, \code{\link{H5Awrite}})
    \item HDF5 Dataspace Interface (\code{\link{H5Screate}}, \code{\link{H5Screate_simple}}, \code{\link{H5Scopy}} / \code{\link{H5Sclose}} / \code{\link{H5Sis_simple}}, \code{\link{H5Sget_simple_extent_dims}} / \code{\link{H5Sselect_hyperslab}})
    \item HDF5 Datatype Interface (\code{\link{H5Tcopy}}, \code{\link{H5Tset_size}})
  }
}

\section{Authors}{
  R-interface by

  Bernd Fischer, \email{bernd.fischer@embl.de}
  EMBL - European Molecular Biology Laboratory
  Heidelberg
  Germany

  The package contains the HDF5 library (\url{http://www.hdfgroup.org/HDF5}).
}

\examples{
h5createFile("ex_hdf5file.h5")

# create groups
h5createGroup("ex_hdf5file.h5","foo")
h5createGroup("ex_hdf5file.h5","foo/foobaa")

# write a matrix
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "ex_hdf5file.h5","foo/B")

# read a matrix
E = h5read("ex_hdf5file.h5","foo/B")

# list content of hdf5 file
h5ls("ex_hdf5file.h5")

# write and read submatrix
h5createDataset("ex_hdf5file.h5", "foo/S", c(5,8), storage.mode = "integer", chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file="ex_hdf5file.h5", name="foo/S", index=list(NULL,1))
h5read("ex_hdf5file.h5", "foo/S")
h5read("ex_hdf5file.h5", "foo/S", index=list(2:3,2:3))
}

\keyword{package}
