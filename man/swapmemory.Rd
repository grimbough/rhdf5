\name{swapmemory}
\alias{swapmemory}
\alias{addmemory}
\alias{hasmemory}
\alias{getmemory}
\alias{freememory}
\title{Functions to handle adding R memory to HDF5 objects. }
\description{
  These functions manipulate memory that is stored using the
  protected slot in the external reference.
}
\usage{
swapmemory(set1, set2)
addmemory(set1)
freememory(set1)
hasmemory(set1)
getmemory(set1)
}

\arguments{
  \item{set1}{An HDF5 dataset. }
  \item{set2}{An HDF5 dataset. }
}
\details{
  Sometimes, in the interest of efficiency we may want to preallocate
  memory that will subsequently be used to perform calculations on an
  HDF5 data set. For example, if a number of identical images are going
  to be processed by a C/Fortran routine then it will be more efficient
  to allocate one piece of memory and swap it between HDF5 objects.

  These functions provide a primitive interface to that functionality.
}
\value{
  \code{hasmemory} returns \code{TRUE} if the HDF5 dataset has memory
  allocated for it.

  \code{freememory} sets the memory to NULL and the memory that was
  stored will be collected by the garbage collector the next time it
  runs.

  \code{swapmemory} swaps the memory held by \code{set1} to
  \code{set2}. The memory will be NULL for \code{set1} after this
  operation.

  \code{addmemory} adds a vector of R allocated memory that is large
  enough to hold the entire hdf5 object. At some point in time it might
  be useful to have the option of allocating for a selected dataspace.
  
  \code{getmemory} returns the memory, as a vector. It may not be
  initialized. This could be an option at some later date.
}
\author{R. Gentleman}

\seealso{  \code{\link{hdf5}} }

\examples{
#we need a testing paradigm

}
\keyword{manip}
