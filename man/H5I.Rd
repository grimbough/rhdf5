\name{HDF5 Identifier Interface}
\alias{H5I}
\alias{H5Iget_type}
\title{HDF5 Identifier Interface}
\description{These functions provides tools for working with object identifiers and object names.}
\usage{
H5Iget_type(h5obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5obj}{An object of class \code{\link{H5file}}, class \code{\link{H5group}} or class \code{\link{H5dataset}}. See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, \code{\link{H5Dopen}} to create an object of this kind.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5O.html} for further details.
}
\value{\code{H5Iget_type} returns the type of an object.}
\references{\url{http://www.hdfgroup.org/HDF5}}
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
