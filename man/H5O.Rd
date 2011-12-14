\name{HDF5 Object Interface}
\alias{H5O}
\alias{H5Oget_info}
\alias{H5Oget_info_by_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Link Interface}
\description{
The Link interface, H5L, functions create and manipulate links in an HDF5 group. This interface includes functions that enable the creation and use of user-defined link classes.
}
\usage{
H5Oget_info         (h5obj)
H5Oget_info_by_name (h5loc, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5obj}{An object of class \code{\link{H5file}}, class \code{\link{H5group}} or class \code{\link{H5dataset}}. See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, \code{\link{H5Dopen}} to create an object of this kind.}
  \item{h5loc}{An object of class \code{\link{H5file}} or class \code{\link{H5group}}. See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{name}{The name of the link to be checked.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5LO.html} for further details.
}
\value{
\code{H5Oget_info} and \code{H5Oget_info_by_name} return a list with the entries of the C-structure \code{H5O_info_t}.
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
H5Oget_info_by_name(fid, "foo")
H5Oget_info_by_name(fid, "foo/B")
did <- H5Dopen(fid, "foo/B")
H5Oget_info(did)
H5Dclose(did)
H5Fclose(fid)

}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
