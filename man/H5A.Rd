\name{HDF5 Attribute Interface}
\alias{H5A}
%\alias{H5Acreate}
%\alias{H5Aclose}
%\alias{H5Adelete}
%\alias{H5Aexists}
\alias{H5Aget_name}
\alias{H5Aget_space}
\alias{H5Aget_type}
%\alias{H5Aopen}
%\alias{H5Aopen_by_idx}
%\alias{H5Aopen_by_name}
\alias{H5Aread}
\alias{H5Awrite}
\title{HDF5 Attribute Interface}
\description{These functions create and manipulate attributes and information about attributes. }
\usage{
H5Acreate       (h5obj, name, dtype_id, h5space)
H5Aclose        (h5attribute)
H5Adelete       (h5obj, name)
H5Aexists       (h5obj, name)
H5Aget_name     (h5attribute)
H5Aget_space    (h5attribute)
H5Aget_type     (h5attribute)
H5Aopen         (h5obj, name)
H5Aopen_by_idx  (h5obj, n, objname = ".", index_type = h5default("H5_INDEX"), 
                 order = h5default("H5_ITER"))
H5Aopen_by_name (h5obj, objname = ".", name)
H5Aread         (h5attribute, buf = NULL)
H5Awrite        (h5attribute, buf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5obj}{An object of class \code{\link{H5IdComponent}} representing a H5 object identifier (file, group, or dataset). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of this kind.}
  \item{name}{The name of the attribute (character).}
  \item{dtype_id}{A character name of a datatype. See \code{h5const("H5T")} for possible datatypes. Can also be an integer representing an HDF5 datatype. Only simple datatypes are allowed for atttributes.}
  \item{h5space}{An object of class \code{\link{H5IdComponent}} representing a H5 dataspace. See \code{\link{H5Dget_space}}, \code{\link{H5Screate_simple}}, \code{\link{H5Screate}} to create an object of this kind.}
  \item{h5attribute}{An object of class \code{\link{H5IdComponent}} represnting a H5 attribute as created by \code{H5Acreate} or \code{H5Aopen}}
  \item{n}{Opens attribute number n in the given order and index. The first attribute is opened with n=0.}
  \item{objname}{The name of the object the attribute belongs to.}
  \item{index_type}{See \code{h5const("H5_INDEX")} for possible arguments.}
  \item{order}{See \code{h5const("H5_ITER")} for possible arguments.}
  \item{buf}{Reading and writing buffer containing the data to written/read. When using the buffer for reading, the buffer size has to fit the size of the memory space \code{h5spaceMem}. No extra memory will be allocated for the data. A pointer to the same data is returned.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{https://portal.hdfgroup.org/display/HDF5/Attributes} for further details.
}
\value{
\code{H5Acreate}, \code{H5Aopen}, \code{H5Aopen_by_name}, \code{H5Aopen_by_idx}  return an object of class \code{\link{H5IdComponent}} representing a H5 attribute identifier.

\code{H5Aget_space} returns an object of class \code{\link{H5IdComponent}} representing a H5 dataspace identifier.

\code{H5Aread} returns an array with the read data.

The other functions return the standard return value from their respective C-functions.
}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
# create a file and write something
h5createFile("ex_H5A.h5")
h5write(1:15, "ex_H5A.h5","A")

# write an attribute 'unit' to 'A'
fid <- H5Fopen("ex_H5A.h5")
did <- H5Dopen(fid, "A")
sid <- H5Screate_simple(c(1,1))
tid <- H5Tcopy("H5T_C_S1")

H5Tset_size(tid, 10L)
aid <- H5Acreate(did, "unit", tid, sid)
aid
H5Awrite(aid, "liter")
H5Aclose(aid)
H5Sclose(sid)
H5Aexists(did, "unit")
H5Dclose(did)
H5Fclose(fid)
h5dump("ex_H5A.h5")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }

