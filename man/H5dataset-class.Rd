\name{H5dataset-class}
\Rdversion{1.1}
\docType{class}
\alias{H5dataset-class}
\alias{H5dataset}
\alias{show,H5dataset-method}

\title{Class "H5dataset"}
\description{
A class representing a HDF5 dataset handle.
}
\section{Objects from the Class}{
Objects can be created by calls of \code{\link{H5Dcreate}} or \code{\link{H5Dopen}}.
}
\section{Slots}{
  \describe{
    \item{\code{ID}:}{Object of class \code{"integer"}: Contains the handle of C-type hid_t. }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "H5dataset")}: Shows the filename, groupname, datatype, and dimensions. }
	 }
}
\references{\url{http://www.hdfgroup.org/HDF5}}
\author{Bernd Fischer}
\seealso{
\link{rhdf5}
}
\examples{
showClass("H5dataset")
}
\keyword{classes}
