\name{HDF5 Dataset Interface}
\alias{H5D}
\alias{H5Dcreate}
\alias{H5Dopen}
\alias{H5Dclose}
\alias{H5Dget_space}
\alias{H5Dread}
\alias{H5Dwrite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Dataset Interface}
\description{
These functions create and manipulate dataset objects, and set and retrieve their constant or persistent properties.
}
\usage{
H5Dcreate    (h5loc, name, dtype_id, h5space, internal1 = NULL, internal2 = 6)
H5Dopen      (h5loc, name)
H5Dclose     (h5dataset)
H5Dget_space (h5dataset)
H5Dread      (h5dataset, h5spaceFile = NULL, h5spaceMem = NULL, buf = NULL, compoundAsDataFrame = TRUE)
H5Dwrite     (h5dataset, buf, h5spaceMem = NULL, h5spaceFile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5loc}{An object of class \code{\link{H5file}} or class \code{\link{H5group}}. See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{name}{Name of the dataset (character).}
  \item{dtype_id}{A character name of a datatype. See \code{h5const("H5T")} for possible datatypes. Can also be an integer representing an HDF5 datatype.}
  \item{h5space}{An object of class \code{\link{H5space}}. See \code{\link{H5Dget_space}}, \code{\link{H5Screate_simple}}, \code{\link{H5Screate}} to create an object of this kind.}
  \item{h5dataset}{An object of class \code{\link{H5dataset}}. See \code{\link{H5Dcreate}}, \code{\link{H5Dopen}} to create an object of this kind.}
  \item{h5spaceFile,h5spaceMem}{An object of class \code{\link{H5space}}. See \code{\link{H5Dget_space}}, \code{\link{H5Screate_simple}}, \code{\link{H5Screate}} to create an object of this kind. The dimensions of the dataset in the file and in memory. The dimensions in file and in memory are interpreted in an R-like manner. The first dimension is the fastest changing dimension. When reading the file with a C-program (e.g. HDFView) the order of dimensions will invert, because in C the fastest changing dimension is the last one.}
  \item{buf}{Reading and writing buffer containing the data to written/read. When using the buffer for reading, the buffer size has to fit the size of the memory space \code{h5spaceMem}. No extra memory will be allocated for the data. A pointer to the same data is returned.}
  \item{compoundAsDataFrame}{If true, a compound datatype will be coerced to a data.frame. This is not possible, if the dataset is multi-dimensional. Otherwise the compound datatype will be returned as a list. Nested compound data types will be returned as a nested list.}
  \item{internal1,internal2}{For internal usage only. Will be removed in later versions.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5D.html} for further details.
}
\value{
\code{H5Dcreate} and \code{H5Dopen} return an object of class \code{\link{H5dataset}}.

\code{H5Dget_space} returns an object of class \code{\link{H5space}}.

\code{H5Dread} returns an array with the read data.

The other functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
# write a dataset
fid <- H5Fcreate("ex_H5D.h5")
fid
sid <- H5Screate_simple(c(10,5,3))
sid
did <- H5Dcreate(fid, "A", "H5T_STD_I32LE", sid)
did
H5Dwrite(did, 1L:150L, h5spaceMem = sid, h5spaceFile = sid)
H5Dclose(did)
H5Sclose(sid)
H5Fclose(fid)

# read a dataset
fid <- H5Fopen("ex_H5D.h5")
fid
did <- H5Dopen(fid, "A")
did
sid <- H5Dget_space(did)
sid
B <- H5Dread(did)
B
H5Dclose(did)
H5Sclose(sid)
H5Fclose(fid)

# write a subarray
fid <- H5Fopen("ex_H5D.h5")
fid
did <- H5Dopen(fid, "A")
did
sid <- H5Dget_space(did)
sid
H5Sselect_index(sid, list(1:3,2:4,2))
sidmem <- H5Screate_simple(c(3,3,1))
sidmem
A = array(-801:-809,dim=c(3,3,1))
H5Dwrite(did, A, h5spaceMem = sidmem, h5spaceFile = sid)
H5Dread(did)
H5Sclose(sid)
H5Dclose(did)
H5Sclose(sidmem)
H5Fclose(fid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
