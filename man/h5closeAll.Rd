\name{h5closeAll}
\alias{h5closeAll}

\title{Close all open HDF5 handles}

\description{Occasionally references to HDF5 files, group, obejects etc can be
created and not closed correctly.  This function identifies all open handles 
and closes them.}

\usage{
h5closeAll()
}

\details{
Creates an empty HDF5 file.
}


\author{Mike Smith}

\examples{

## create an empty file and then re-open it
h5createFile("ex_h5closeAll.h5")
H5Fopen("ex_h5closeAll.h5")

## list all open identifiers
h5listIdentifier()

## close all open identifiers and verify
h5closeAll()
h5listIdentifier()
}
\keyword{ IO }
\keyword{ file }
