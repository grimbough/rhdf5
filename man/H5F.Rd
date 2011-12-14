\name{HDF5 File Interface}
\alias{H5F}
\alias{H5Fcreate}
\alias{H5Fopen}
\alias{H5Freopen}
\alias{H5Fclose}
\alias{H5Fflush}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 File Interface}
\description{
These functions are designed to provide file-level access to HDF5 files. 
}
\usage{
H5Fcreate (name, flags = h5default("H5F_ACC"))
H5Fopen   (name, flags = h5default("H5F_ACC_RD"))
H5Freopen (h5file)
H5Fclose  (h5file)
H5Fflush  (h5file, scope = h5default("H5F_SCOPE"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{The filename of the HDF5 file.}
  \item{flags}{See \code{h5const("H5F_ACC")} for possible arguments.}
  \item{h5file}{An object of class \code{\link{H5file}} as created by \code{H5Fcreate} or \code{H5Fopen}.}
  \item{scope}{See \code{h5const("H5F_ACC_RD")} for possible arguments.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html} for further details.
}
\value{
\code{H5Fcreate}, \code{H5Fopen}, and \code{H5Freopen} return an object of class \code{\link{H5file}}.

The other functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
fid <- H5Fcreate("ex_H5F.h5")
fid
H5Fclose(fid)
fid2 <- H5Fopen("ex_H5F.h5")
fid2
H5Fclose(fid2)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
