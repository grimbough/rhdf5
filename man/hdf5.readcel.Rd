\name{hdf5.readcel}
\alias{hdf5.readcel}
\title{  Read and Affymetrix CEL file into an HDF5 file}
\description{
  Reads data in the Affymetrix CEL file format into an HDF5 dataset.
}
\usage{
hdf5.readcel(set, file, create=hdf5.default.properties)
}
\arguments{
  \item{set}{ An HDF5 group that will contain the CEL file.}
  \item{file}{ The name of the file on disk. }
  \item{create}{The properties to be used when creating the HDF5 dataset. }
}
\details{
  Three datasets are created in the group. They are Mean, Stdev and
  Npix, corresponding to the three pieces of information provided by
  Affymetrix. 
}
\value{
 No value is returned. The function is executed for the side effect of
 creating the HDF5 data sets.
}

\author{B. Ellis }

\seealso{\code{\link{hdf5}}}

\examples{
#no good way to do this....
}
\keyword{ manip }
