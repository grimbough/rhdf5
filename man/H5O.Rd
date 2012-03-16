\name{HDF5 Object Interface}
\alias{H5O}
\alias{H5Oopen}
\alias{H5Oclose}
\alias{H5Oget_num_attrs}
\alias{H5Oget_num_attrs_by_name}
%\alias{H5Oget_info}
%\alias{H5Oget_info_by_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Object Interface}
\description{
The Object interface, H5O, functions manipulate objects in an HDF5 file. This interface is designed to be used in conjunction with the Links interface (H5L).
}
\usage{
H5Oopen  (h5loc, name)
H5Oclose (h5obj)
H5Oget_num_attrs(h5obj)
H5Oget_num_attrs_by_name(h5loc, name)
}
% H5Oget_info         (h5obj)
% H5Oget_info_by_name (h5loc, name)
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5obj}{An object of class \code{\link{H5IdComponent}} representing a H5 object identifier (file, group, or dataset). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of this kind.}
  \item{h5loc}{An object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{name}{The name of the link to be checked.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5O.html} for further details.
}
\value{
\code{H5Oopen} opens an object (a file, group, or dataset) and returns an object of class \code{\link{H5IdComponent}}. \code{H5Oclose} closed the object again. \code{H5Oget_num_attrs} and \code{H5Oget_num_attrs_by_name} return the number of attributes of an object.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
# create an hdf5 file and write something
h5createFile("ex_H5O.h5")
h5createGroup("ex_H5O.h5","foo")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
h5write(B, "ex_H5O.h5","foo/B")

# reopen file and dataset and get object info
fid <- H5Fopen("ex_H5O.h5")
oid = H5Oopen(fid, "foo")
H5Oget_num_attrs(oid)
H5Oclose(oid)
H5Fclose(fid)

}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
