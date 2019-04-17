\name{HDF5 Identifier Interface}
\alias{H5I}
\alias{H5Iget_type}
\alias{H5Iget_name}
\alias{H5Iis_valid}
\title{HDF5 Identifier Interface}
\description{These functions provides tools for working with object identifiers and object names.}
\usage{
H5Iget_type(h5identifier)
H5Iget_name(h5obj)
H5Iis_valid(h5identifier)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5identifier}{An object of class \code{\link{H5IdComponent}} representing a H5 identifier (file, group, dataset, dataspace, datatype, attribute). See e.g. \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, \code{\link{H5Dopen}} to create an object of this kind.}
  \item{h5obj}{An object of class \code{\link{H5IdComponent}} representing a H5 object identifier (file, group, or dataset). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of this kind.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{https://portal.hdfgroup.org/display/HDF5/Identifiers} for further details.
}
\value{\code{H5Iget_type} returns the type of the H5 identifier, \code{H5Iget_name} the name of the object, and \code{H5Iis_valid} checks if the object is a valid H5 identifier. }
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
# create an hdf5 file and write something
h5createFile("ex_H5I.h5")
h5createGroup("ex_H5I.h5","foo")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
h5write(B, "ex_H5I.h5","foo/B")

# reopen file and dataset and get object info
fid <- H5Fopen("ex_H5I.h5")
oid = H5Oopen(fid, "foo")
H5Iget_type(oid)
H5Oclose(oid)
H5Fclose(fid)

}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
