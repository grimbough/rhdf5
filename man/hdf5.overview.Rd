\name{hdf5.overview}
\alias{hdf5}
\title{HDF5 Interface Library for R}
\description{
  This library strives to bring more complete HDF5 support to the R
  environment in a way that is hopefully natural to the 
  R/S paradigm for working with data objects. 
}
\examples{
\dontrun{  # Load the HDF5 library 
  library(hdf5)
  # Attach open or create an HDF5 file to the variable "mad."
  mad <- hdf5.open("microarray.h5")
  # Create a new group inside the file
  hdf5.group(mad,"chip1")
  hdf5.dataset(mad$chip1,"raw", dim=c(534,534))

  raw <- mad$chip1$raw
  for(i in 1:534) 
      { raw[i,1:534] <- runif(534) }

 # Draw an image of our chip. About 17megs if you try it! 
  image(raw[1:534,1:534])        
}}
\seealso{ \code{\link[hdf5]{hdf5.group}},
  \code{\link[hdf5]{hdf5.dataset}}}

\keyword{documentation}
