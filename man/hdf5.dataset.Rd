\name{hdf5.dataset}
\alias{hdf5.dataset}
\title{HDF5 Dataset}
\description{
  In the HDF5 hierarchy there are files, groups and datasets. Files
  contain groups which contain datasets. The datasets are the actual
  data the other two levels are organizational.
}
\usage{
hdf5.dataset(group, name, data=NULL, dims=NULL, type=NULL,
                         create=hdf5.default.properties)
}
\arguments{
  \item{group}{An instance of \code{\link{hdf5.file}} or
    \code{hdf5.group}. } 
  \item{name}{A character vector naming the new group. }
  \item{data}{Data to store in the HDF5 dataset. }
  \item{dims}{The dimensions of the dataset to create. }
  \item{type}{The type of dataset to create. See
    \code{link{hdf5.datatypes}} for details. }
  \item{create}{An HDF5 property list. }
}

\details{
Datasets are the actual data storage units in HDF5. This function allows
users to create new data objects. The user can either supply data to be
stored as an HDF5 dataset. In this case the \code{dims} and \code{type}
arguments must not be specified. The \code{type} and \code{create}
arguments are ignored.
If \code{dims} is specified then \code{data} must not be.
The default \code{type} is \texttt{double}.
}

\value{
  An object of class \code{hdf5.dataset} which contains the data, or is
  of the specified type and dimensions is created within the HDF5 group
  or file \code{group}. The object itself is returned invisibly. If it
  is not assigned it can still be retrieved from \code{group}.
}
\author{R. Gentleman and B. Ellis}

\seealso{\code{\link{hdf5.file}}, \code{\link{hdf5.file}}}

\examples{
  mad <- hdf5.open("MarrayXX.h5")
  c1 <- hdf5.group(mad,"chip1")
  hdf5.dataset(c1, "rnorm", rnorm(1000))
  c1
  length(c1$rnorm)
  c1$rnorm
  hdf5.dataset(mad, "x2", dims=c(10,10), type="ushort")
  mad$x2
  mad
  unlink("MarrayXX.h5")
}
\keyword{manip}

