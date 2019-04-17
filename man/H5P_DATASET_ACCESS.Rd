\name{HDF5 Dataset Access Property List Interface}
\alias{H5P_DATASET_ACCESS}
\alias{H5Pset_chunk_cache}
\alias{H5Pget_chunk_cache}
\title{HDF5 Dataset Access Property List Interface}
\description{The functions, macros, and subroutines listed here are used to manipulate dataset access property list objects in various ways, including to reset property values. With the use of property lists, HDF5 functions have been implemented and can be used in applications with many fewer parameters than would be required without property lists.}
\usage{
H5Pset_chunk_cache                 ( h5plist, rdcc_nslots, rdcc_nbytes, rdcc_w0 )
% H5Pget_chunk_cache                 ( dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0 )
}
\arguments{
  \item{h5plist}{An object of class \code{\link{H5IdComponent}} representing a H5 property list identifier of class H5P_DATASET_ACCESS. See \code{\link{H5Pcreate}} or \code{\link{H5Pcopy}} to create an object of this kind.}
  \item{rdcc_nslots}{An integer. The number of chunk slots in the raw data chunk cache for this dataset.}
  \item{rdcc_nbytes}{An integer. The total size of the raw data chunk cache for this dataset.}
  \item{rdcc_w0}{double. The chunk preemption policy for this dataset.}
}
\details{
Interface to the HDF5 C-library libhdf5. See \url{https://portal.hdfgroup.org/display/HDF5/Dataset+Access+Properties} for further details. See \link{H5P} for documentation of more functions to manupulate property lists of other property list classes. 
}
\value{
The functions return the standard return value from their respective C-functions.
}
\references{\url{https://portal.hdfgroup.org/display/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}, \link{H5P}
}
\examples{
pid <- H5Pcreate("H5P_DATASET_ACCESS")
H5Pset_chunk_cache( pid, 100, 10000, 0.5)
H5Pclose(pid)
}
\keyword{ programming }
\keyword{ interface }
\keyword{ IO }
\keyword{ file }
