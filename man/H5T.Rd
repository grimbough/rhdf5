\name{HDF5 Datatype Interface}
\alias{H5T}
\alias{H5Tcopy}
\alias{H5Tset_size}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 Datatype Interface}
\description{
These functions create and manipulate the datatype which describes elements of a dataset. 
}
\usage{
H5Tcopy     (dtype_id = h5default(type = "H5T"))
H5Tset_size (dtype_id = h5default(type = "H5T"), size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dtype_id}{A character name of a datatype. See \code{h5const("H5T")} for possible datatypes. Can also be an integer representing an HDF5 datatype.}
  \item{size}{The total size in bytes.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5T.html} for further details.
}
\value{
The functions return the standard return value from their respective C-functions.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
# create character datatype with string length 10
tid <- H5Tcopy("H5T_C_S1")
H5Tset_size(tid, 10L)

# List all predefined types implemented in the R-interface
h5const("H5T")

# List all available type classes (not all of them are implemented)
h5const("H5T_CLASS")
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
