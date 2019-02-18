\name{HDF5 General Library Functions}
\alias{H5}
\alias{H5open}
\alias{H5close}
\alias{H5garbage_collect}
\alias{H5get_libversion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HDF5 General Library Functions}
\description{
These low level functions provide general general libary functions for HDF5.
}
\usage{
H5open             ()
H5close            ()
H5garbage_collect  ()
H5get_libversion   ()
}
\details{
These low level functions provide general general libary functions for HDF5 C-library libhdf5. See \url{http://www.hdfgroup.org/HDF5/doc/RM/RM_H5.html} for further details.
}
\value{
\code{H5open} initializes the HDF5 library. \code{H5close} flushes all data to disk, closes all open identifiers, and cleans up memory. \code{H5garbage_collect} cleans up memory. \code{H5get_libversion} returns the version number of the HDF5 C-library.
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\code{\link{h5version}}, \link{rhdf5}
}
\examples{

# test for file locking
if( !h5testFileLocking( tempdir() ) ) {
      h5disableFileLocking()
}

H5open()
H5close()
H5garbage_collect()
H5get_libversion()
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
