\name{hdf5.info}
\alias{hdf5.info}
\alias{print.hdf5.info}
\alias{groupgetinfo}
\title{HDF5 Info}
\description{
  In the HDF5 hierarchy there are files, groups and datasets. Files
  contain groups which contain datasets. The datasets are the actual
  data the other two levels are organizational. This class and its
  associated methods provide an R level interface to these values.
}
\usage{
print.hdf5.info(x, ...)
groupgetinfo(group, name)
}
\arguments{
  \item{x}{An object of class \code{hdf5.info}.}
  \item{group}{An instance of \code{\link{hdf5.file}} or
    \code{hdf5.group}. } 
  \item{name}{A character vector naming the new group. }
  \item{\dots}{Optional arguments}
}
\details{
  Currently three values are returned. The \code{type} which is one of
  group, type, link, type or default. The \code{modification time} which
  is the last time this HDF5 object was modified. The number of hard
  links to the object, \code{Links}.
}

\value{
  \code{print.hdf5.group} prints the values and returns \code{x}
  invisibly.
  \code{groupgetinfo} returns an object of class \code{hdf5.info} that
  contains the three values listed above.
}
\references{\samp{H5Gget_objinfo}}
\author{R. Gentleman and B. Ellis}

\seealso{\code{\link{hdf5.group}}}

\examples{
 \dontrun{ mad <- hdf5.open("microarray.h5")
  hdf5.group(mad,"chip1")
  hdf5.group(mad$chip1,"group")
  groupgetinfo(mad$chip1, "group")
}
}
\keyword{manip}
