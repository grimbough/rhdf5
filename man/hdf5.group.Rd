\name{hdf5.group}
\alias{hdf5.group}
\alias{[.hdf5.group}
\alias{[[.hdf5.group}
\alias{$.hdf5.group}
\alias{hdf5.group.rm}
\title{HDF5 Group}
\description{
  In the HDF5 hierarchy there are files, groups and datasets. Files
  contain groups which contain datasets. The datasets are the actual
  data the other two levels are organizational.
}
\usage{
hdf5.group(group, name)
x[i]
x[i,j, ...]
x[i,j, ..., drop=TRUE]
x[[i]]
hdf5.group.rm(group, name)
}
\arguments{
  \item{x}{An object of class \code{hdf5.group}.}
  \item{i}{An index into the object.}
  \item{j}{An index into the object.}
  \item{\dots}{ Optional arguments. }
  \item{drop}{A logical value, if \code{TRUE} the result may drop
    dimensions.} 

  \item{group}{An instance of \code{\link{hdf5.file}} or
    \code{hdf5.group}. } 
  \item{name}{A character vector naming the new group. }
}
\details{
  A \em{group} in the HDF5 since is merely an organizational
  structure. The basic unit is an HDF5 file, which contains, groups and
  groups contain datasets. It is possible for a dataset to belong to
  more than one group.

  The single-bracket subset operator,
  \code{[}, is not implemented. It should return a new group, but since
  there is no way to name that group, it is not clear what to do.

  \code{hdf.group.rm} removes (or unlinks) the HDF5 object named
  \code{name} from the HDF5 object \code{group}. Note that caution
  should be used since it is possible to make data inaccessible by
  removing all links to it.
}
\value{
  The \code{[[} and \code{$} operators return the named element of a
  group; which can be either an \code{hdf5.group} or an
  \code{hdf5.dataset}.

  \code{hdf.group.rm} returns its first argument (\code{group}),
  invisibly. 
}

\author{R. Gentleman and B. Ellis}

\seealso{\code{\link{hdf5.file}}}

\examples{
 \dontrun{ mad <- hdf5.open("microarray.h5")
  hdf5.group(mad,"chip1")
  hdf5.group(mad$chip1,"group")
  hdf5.group.rm(mad$chip1,"group")
}
}
\keyword{manip}
