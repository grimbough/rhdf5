\name{h5delete}
\alias{h5delete}
\alias{h5deleteAttribute}

\title{Delete objects within a HDF5 file}

\description{Deletes the specfied group, dataset or attribute from within an HDF5 file.}

\usage{
    h5delete( file, name )
    h5deleteAttribute( file, name, attribute )
}

\arguments{
  \item{file}{The filename (character) of the file in which the object is located.}
  \item{name}{For \code{h5delete} the name of the object to be deleted.
For \code{h5deleteAttribute} the name of the object to which the attribute belongs.}
  \item{attribute}{Name of the attribute to be deleted.}
}

\author{Mike Smith}

\keyword{ file }
