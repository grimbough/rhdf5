\name{H5file-class}
\Rdversion{1.1}
\docType{class}
\alias{H5file-class}
\alias{H5file}
\alias{show,H5file-method}

\title{Class "H5file"}
\description{
A class representing a HDF5 file handle.
}
\section{Objects from the Class}{
Objects can be created by calls of \code{\link{H5Fcreate}}, \code{\link{H5Fopen}}, or \code{\link{H5Freopen}}.
}
\section{Slots}{
  \describe{
    \item{\code{ID}:}{Object of class \code{"integer"}. Contains the handle of C-type hid_t. }
  }
}
\section{Methods}{
  \describe{
	\item{show}{\code{signature(object = "H5file")}: Shows the filename. }
    }
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
showClass("H5file")
}
\keyword{classes}
