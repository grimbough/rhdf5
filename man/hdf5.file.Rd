\name{hdf5.file}
\alias{hdf5.file}
\alias{hdf5.open}
\alias{hdf5.create}
\alias{[[.hdf5.file}
\alias{[.hdf5.file}
\title{HDF5 File Interface}
\description{
  Files are pretty simple things in HDF5 and inherit most of their
  functionality from the HDF5 group interfaces. The only 
  things unique to the file interface are the open/create/attach commands.
}
\usage{
  hdf5.create(filename,mode="e", create=hdf5.default.properties,
access=hdf5.default.properties)   
  hdf5.open(filename,access=hdf5.default.properties)
  x[[i]]
  x[i]
}
\arguments{
  \item{filename}{A filename.}
  \item{mode}{"e" is for exclusive access (file creation will fail if
    the file exists). "t" is to truncate a preexisting file.} 
  \item{create}{A creation property list (don't touch it for the moment)}
  \item{access}{An access property list (hands off!)}
  \item{x}{An object of class \code{hdf5.file}}
  \item{i}{The index of an object in \code{x}. This may be either
    numeric or character}  
}
\value{
  Returns a reference to the file. \code{hdf5.create} creates a new HDF5
  file. It signals an error if the file already exists.
  \code{hdf5.open} opens an existing file or creates a file with the
  specified name.

  The function \code{[.hdf5.file} throws an error. It is not clear how
  to implement such a function.
  
}
\seealso{\code{\link[hdf5]{hdf5.groups}},
  \code{\link[hdf5]{hdf5.create.group}},
  \code{\link[hdf5]{hdf5.create.dataset}}} 
\examples{
  mad <- hdf5.open("marrayXX.h5")
  hdf5.group(mad,"chip1")            # Make a new group inside of mad
  mad
  mad[[1]]

  mad[["chip1"]]                           
  unlink("marrayXX.h5")
}
\keyword{classes}
