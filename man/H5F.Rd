\name{HDF5 File Interface}
\alias{H5F}
\alias{H5Fcreate}
\alias{H5Fopen}
\alias{H5Fclose}
\alias{H5Fflush}
\alias{H5Fis_hdf5}
\alias{H5Fget_filesize}
\alias{H5Fget_name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 File Interface}
\description{
These functions are designed to provide file-level access to HDF5 files. 
}
\usage{
H5Fcreate      (name, flags = h5default("H5F_ACC"), fcpl = NULL, fapl = NULL)
H5Fopen        (name, flags = h5default("H5F_ACC_RD"))
H5Fclose       (h5file)
H5Fflush       (h5file, scope = h5default("H5F_SCOPE"))
H5Fis_hdf5     (name, showWarnings = TRUE)
H5Fget_filesize(h5file)
H5Fget_name    (h5obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{The filename of the HDF5 file.}
  \item{flags}{See \code{h5const("H5F_ACC")} for possible arguments.}
  \item{fcpl}{An object of class \code{\link{H5IdComponent}} representing a H5 file creation property list. See \code{\link{H5Pcreate}}, \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{fapl}{An object of class \code{\link{H5IdComponent}} representing a H5 file access property list. See \code{\link{H5Pcreate}}, \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{h5file}{An object of class \code{\link{H5IdComponent}} representing a H5 file identifier as created by \code{H5Fcreate} or \code{H5Fopen}.}
  \item{scope}{See \code{h5const("H5F_ACC_RD")} for possible arguments.}
  \item{showWarnings}{If TRUE (default), a warning is given if an open HDF5 handle exists for this file.}
  \item{h5obj}{An object of class \code{\link{H5IdComponent}} representing a H5 object identifier (file, group, or dataset). See \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, \code{\link{H5Gcreate}}, \code{\link{H5Gopen}}, \code{\link{H5Dcreate}}, or \code{\link{H5Dopen}} to create an object of this kind.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html} for further details.
}
\value{
\code{H5Fcreate} and \code{H5Fopen} return an object of class \code{\link{H5IdComponent}} representing a H5 file identifier.

\code{H5Fis_hdf5} returns TRUE, if the file is an HDF5 file, or FALSE otherwise. In the case the file doesn't exist, NA is returned.

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
