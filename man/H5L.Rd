\name{HDF5 Link Interface}
\alias{H5L}
\alias{H5Lexists}
\alias{H5Lget_info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Link Interface}
\description{
The Link interface, H5L, functions create and manipulate links in an HDF5 group. This interface includes functions that enable the creation and use of user-defined link classes.
}
\usage{
H5Lexists   (h5loc, name)
H5Lget_info (h5loc, name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{h5loc}{An object of class \code{\link{H5IdComponent}} representing a H5 location identifier (file or group). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}} to create an object of this kind.}
  \item{name}{The name of the link to be checked.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5L.html} for further details.

If \code{name} consists of a relative path containing group names, the function \code{H5Lexists} checks recursively if the links exists which is a different behaviour to the C-function.
}
\value{
\code{H5Lexists} returns boolean TRUE if the link exists and FALSE otherwise.

\code{H5Lget_info} returns a list with the entries of the C-structure \code{H5L_info_t}.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
# create an hdf5 file and a group
h5createFile("ex_H5L.h5")
h5createGroup("ex_H5L.h5","foo")

# reopen file and get link info
fid <- H5Fopen("ex_H5L.h5")
H5Lexists(fid, "foo")
H5Lexists(fid, "baa")
H5Lget_info(fid, "foo")
H5Fclose(fid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
