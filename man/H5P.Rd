\name{HDF5 Property List Interface}
\alias{H5P}
\alias{H5Pcreate}
\alias{H5Pcopy}
\alias{H5Pget_class}
\alias{H5Pclose}
\alias{H5Pclose_class}
\alias{H5Pequal}
\title{HDF5 Property List Interface}
\description{The functions, macros, and subroutines listed here are used to manipulate property list objects in various ways, including to reset property values. With the use of property lists, HDF5 functions have been implemented and can be used in applications with many fewer parameters than would be required without property lists.}
\usage{
H5Pcreate            (type = h5default("H5P"))
H5Pcopy              (h5plist)
H5Pget_class         (h5plist)
H5Pclose             (h5plist)
H5Pclose_class       (h5plistclass)
H5Pequal             (h5plistclass1, h5plistclass2)
H5Pset_libver_bounds (h5plist, libver_low = "H5F_LIBVER_18", libver_high = "H5F_LIBVER_LATEST")
H5Pget_libver_bounds (h5plist)
}
\arguments{
  \item{type}{A character name of a property list type. See \code{h5const("H5P")} for possible property list types. Can also be an integer representing an HDF5 property list type.}
  \item{h5plist}{An object of class \code{\link{H5IdComponent}} representing a H5 property list identifier. See \code{\link{H5Pcreate}} or \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{h5plistclass, h5plistclass1, h5plistclass2}{An object of class \code{\link{H5IdComponent}} representing a H5 property list class identifier. See \code{\link{H5Pget_class}} to create an object of this kind.}
  \item{libver_low}{A character value representing the lower bound on the library version for reading or writing the HDF5 file. See \code{h5const("H5F_LIBVER")} for possible arguments. Default is "H5F_LIBVER_18"}
  \item{libver_high}{A character value representing the higher bound on the library version for reading or writing the HDF5 file. See \code{h5const("H5F_LIBVER")} for possible arguments. Default is "H5F_LIBVER_LATEST"}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html} for further details.
}
\value{
\code{H5Pcreate} and \code{H5Pcopy} return an object of class \code{\link{H5IdComponent}} representing a H5 property list identifier.

\code{H5Pget_class} returns an object of class \code{\link{H5IdComponent}} representing a H5 property list class identifier.

The other functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}, \link{H5P_DATASET_CREATE}
}
\examples{
pid <- H5Pcreate()
pid2 <- H5Pcopy(pid)
pclid <- H5Pget_class(pid)
H5Pclose_class(pclid)
H5Pclose(pid)
H5Pclose(pid2)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
