\name{HDF5 Link Create Property List Interface}
\alias{H5P_LINK_CREATE}
\alias{H5Pset_char_encoding}
\alias{H5Pget_char_encoding}
\alias{H5Pset_create_intermediate_group}
\alias{H5Pget_create_intermediate_group}
\title{HDF5 Link Create Property List Interface}
\description{The functions, macros, and subroutines listed here are used to manipulate link creation property list objects in various ways, including to reset property values. With the use of property lists, HDF5 functions have been implemented and can be used in applications with many fewer parameters than would be required without property lists.}
\usage{
H5Pset_char_encoding               ( h5plist, encoding  = h5default("H5T_CSET"))
% H5Pget_char_encoding               ( plist, encoding )
H5Pset_create_intermediate_group   ( h5plist, crt_intermed_group )
% H5Pget_create_intermediate_group   ( lcpl_id, crt_intermed_group )
}
\arguments{
  \item{h5plist}{An object of class \code{\link{H5IdComponent}} representing a H5 property list identifier of class H5P_LINK_CREATE. See \code{\link{H5Pcreate}} or \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{encoding}{A character name of an encoding type. See \code{h5const("H5T_CSET")} for possible property list types. Can also be an integer representing an HDF5 encoding type.}
  \item{crt_intermed_group}{An integer. If niot zero, intermediate groups are created.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5P.html} for further details. See \link{H5P} for documentation of more functions to manupulate property lists of other property list classes. 
}
\value{
The functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}, \link{H5P}
}
\examples{
pid <- H5Pcreate("H5P_LINK_CREATE")
H5Pset_char_encoding( pid, "H5T_CSET_ASCII")
H5Pset_create_intermediate_group(pid, 1L)
H5Pclose(pid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
