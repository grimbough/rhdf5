\name{H5space-class}
\Rdversion{1.1}
\docType{class}
\alias{H5space-class}
\alias{H5space}
\alias{show,H5space-method}

\title{Class "H5space"}
\description{
A class representing a HDF5 dataspace handle.
}
\section{Objects from the Class}{
Objects can be created by calls of \code{\link{H5Screate}}, \code{\link{H5Screate_simple}}, \code{\link{H5Scopy}}, or \code{\link{H5Dget_space}}.
}
\section{Slots}{
  \describe{
    \item{\code{ID}:}{Object of class \code{"integer"}: Contains the handle of C-type hid_t.  }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "H5space")}: Shows the dataspace dimensions. }
	 }
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
showClass("H5space")
}
\keyword{classes}
